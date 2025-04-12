I have been having performance issue with remote development setup using emacs' tramp. I do know that it does mainly depend on underlying ssh connection speed but there are sometimes overhead introduced by emacs. Note that I am also using doom emacs and would like to be aware of the issues caused by additional config done by doom. I am using emacs 30.1 which seems to have some improvements on tramp. Can you compile resources to optimize and debug/troubleshooting techniques to get deeper into this issue to see if this can be actually fixed or is it just the limitation of emacs+tramp?


---

To help you troubleshoot and optimize your Emacs TRAMP remote development setup, could you share a bit more detail on the following?

What kind of remote environment are you connecting to (e.g., Linux server, container, VM)?

Are you using TRAMP mostly for editing files, running compilation, or other tasks?

Are there specific pain points (e.g., slow file listing, delayed save, sluggish Magit, etc.)?

Are you using any specific TRAMP-related configurations or Doom Emacs modules?

Do you use SSH multiplexing or control master sessions?

This will help me compile the most relevant optimizations and debugging strategies.

---

1. Linux server.
2. Mainly editing files and browsing directories (w/ dired).
3. Exactly, slow dired browsing, noticable delays when saving changes to a file, and yes magit can be significantly slow.
4. didn't introduce additional configs and using packages with their default configs done by doom as much as possible
5. didn't do extra configuration to ssh so it should be default:


tramp:
(use-package! tramp
  ;;:ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

here are the modules enabled:
(doom! 
       :completion
       (vertico +icons)           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       (emoji +unicode +github)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :editor
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       snippets          ; my elves. They type so I don't have to

       :emacs
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes

       :term
       eshell            ; the elisp shell that works everywhere
       vterm             ; the best terminal emulation in Emacs

       :tools
       biblio            ; Writes a PhD for you (citation needed)
       debugger          ; FIXME stepping through code, to help you add bugs
       docker
       eval
       lookup              ; navigate your code and its documentation
       magit             ; a git porcelain for Emacs
       pdf               ; pdf enhancements

       :lang
       emacs-lisp        ; drown in parentheses
       json              ; At least it ain't XML
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org               ; organize your plain life in plain text
        +dragndrop
        +jupyter
        +noter
        +pandoc
        +pomodoro
        +present
        )
       (python +pyright)   ; beautiful is better than ugly
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       yaml              ; JSON, but readable

       :config
       (default +bindings +smartparens))


----

Thanks! I’ll put together a set of strategies and resources to help optimize and debug your Emacs TRAMP performance — especially for Dired, Magit, and file saving — and determine whether the limitations you're seeing are inherent to Emacs + TRAMP or can be mitigated.

I'll dive into Emacs 30.1 and Doom Emacs-specific performance tuning, SSH best practices for TRAMP, and methods for profiling slow operations. I’ll also include any TRAMP-specific patches or Doom module interactions that could be affecting performance.

I’ll follow up shortly with a detailed summary.

# Optimizing Emacs TRAMP Performance Over SSH (Emacs 30.1 + Doom Emacs)

## Introduction 
Editing remote files in Emacs via TRAMP can often feel sluggish compared to local editing. TRAMP (Transparent Remote Access, Multiple Protocols) works by executing file operations over SSH, which introduces network latency and overhead for each command. Common symptoms include painfully slow directory listings in Dired, long delays when saving files, and sluggish Magit operations on remote Git repositories. This guide will walk through various techniques to optimize TRAMP performance on Emacs 30.1 (as shipped with TRAMP 2.6.x) and Doom Emacs, focusing on those specific pain points. We’ll cover TRAMP-specific settings, SSH configuration tweaks, Doom Emacs quirks, known pitfalls/bugs, debugging tools, and realistic expectations for TRAMP’s performance.

## Understanding TRAMP Performance Challenges 
TRAMP enables transparent remote editing by funneling file operations through one or more SSH processes. By design, Emacs sends many individual I/O commands sequentially over the SSH connection, each incurring network round-trip delay ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=The%20other%20thing%20that%20makes,blocks%20for%20multiple%20seconds%2C%20etc)) ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=I%20have%20this%2C%20yet%20I,and%20blocking%20on%20their%20completion)). Unlike editors that run a persistent agent on the remote side (e.g. VS Code’s remote server), Emacs handles remote tasks one step at a time on the main thread. This means that high latency or many small operations can quickly add up to noticeable freezes. For example, opening a remote Dired directory may require TRAMP to invoke `ls` and parse output, and iterating over files (especially if Emacs is checking version control status or file metadata) can trigger **dozens of SSH commands**, each blocking the Emacs UI briefly. Similarly, saving a file might involve creating backups, writing the file contents, and updating file attributes, all over the network. Magit, which runs many Git commands, can magnify these effects if each `git` invocation spawns a new remote process. Recognizing these inherent challenges is the first step – some latency is inevitable, but with careful tuning we can minimize unnecessary delays.

## TRAMP Configuration: Caching, Compression and Connection Reuse 
**1. Enable TRAMP Connection Sharing:** Modern TRAMP supports reusing a single SSH connection for multiple operations, to avoid the overhead of re-authenticating each time. By default, TRAMP passes SSH options to enable OpenSSH’s control master feature (a multiplexed connection) – e.g. `-o ControlMaster=auto -o ControlPath='tramp.%C' -o ControlPersist=no` ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=Tramp%3A%20Sending%20command%20%E2%80%98exec%20ssh,done)). In Emacs 28+, the user option `tramp-use-ssh-controlmaster-options` controls this behavior, and in Emacs 30 it has been renamed to `tramp-use-connection-share` (now covering all SSH-based methods) ([What's New in Emacs 30.1? - Mastering Emacs ](https://www.masteringemacs.org/article/whats-new-in-emacs-301#:~:text=match%20at%20L1819%20Rename%20%27tramp,based%20and)). Ensure this option is **enabled** (it is on by default) so that TRAMP will try to reuse one SSH session for all commands on a given host. When properly configured, TRAMP will open **one** SSH process per host and channel all file operations through it, instead of opening a new SSH session for every file action. You can verify reuse by checking the `*Messages*` TRAMP logs – you should see “Opening connection for <host> using ssh...done” only once, rather than repeatedly for every command. If you find TRAMP is still “re-opening” the connection frequently, double-check your settings and see the Doom Emacs notes below (certain modes can inadvertently cause reconnections).

**2. Leverage SSH ControlMaster in your SSH config:** In addition to Emacs’ internal sharing, it’s wise to set up SSH’s control master/persist in your `~/.ssh/config` for all hosts. This provides an extra layer of persistence. For example, you can add: 

```sshconfig
Host *
  ControlMaster auto
  ControlPath ~/.ssh/control/%C
  ControlPersist yes
```

This ensures the first SSH connection to a host stays alive in the background (`ControlPersist yes` keeps it open even after Emacs/TRAMP would normally close it) and subsequent connections reuse it ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=A%20lot%20of%20this%20is,ssh%2Fconfig%20file)). With this, even if Emacs opens multiple SSH processes or one per thread, they multiplex over the same underlying TCP connection, significantly reducing handshake overhead. In practice, Emacs TRAMP already tries to reuse the connection internally, but this config can help in scenarios where TRAMP might otherwise not persist the master. (Note: Emacs’ default `ControlPersist=no` means it won’t leave an orphaned master when Emacs exits, but our `~/.ssh/config` with `yes` will keep it alive – decide based on your security needs. Setting `ControlPersist 600` for 10 minutes is a good compromise to reap benefits without leaving connections open indefinitely.)

**3. Adjust Compression Settings:** SSH compression can sometimes improve throughput on slow links at the cost of CPU. Emacs TRAMP will automatically compress file transfers above a certain size (`tramp-inline-compress-start-size`, default ~10KB) by piping through gzip. If most of your edits involve large files over a slow network, enabling SSH compression globally (`Compression yes` in `~/.ssh/config`) or lowering TRAMP’s threshold can speed up transfers. Conversely, if you mostly edit small files or have a fast network, you might **raise** the `tramp-inline-compress-start-size` (or disable SSH compression for that host) to avoid CPU overhead on tiny saves. This is a minor tweak, but worth noting if file content transfer is a bottleneck (e.g. working with large binaries or logs).

**4. Enable Remote File Name Caching:** Emacs maintains a cache of remote file attributes to avoid redundant SSH calls. Make sure this is in effect by **not** setting `remote-file-name-inhibit-cache` to true. In fact, explicitly set `(setq remote-file-name-inhibit-cache nil)` to allow caching ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=I%20have%20also%20found%20the,up%20to%20work%20pretty%20well)). With caching on, TRAMP will reuse recently retrieved directory listings and file attributes for a short time (10 seconds by default) instead of querying the remote for every operation. This can greatly speed up Dired navigation and successive file accesses. After changing this setting, you can tune the cache expiry via `tramp-cache-duration` if needed, but usually the default is fine. The slight risk is that if the remote filesystem changes outside Emacs, Emacs might not see it immediately; but for most editing workflows the trade-off is worth it.

**5. Disable Version Control Integration on Remotes:** Emacs’ VC mode and related features (like showing Git status in the modeline) can hammer TRAMP with extra operations. It’s often wise to disable VC for remote files entirely. An easy way is to extend `vc-ignore-dir-regexp` to match TRAMP paths, so that VC doesn’t consider remote directories as version-controlled projects ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=%28setq%20remote,regexp)). For example: 

```elisp
(setq vc-ignore-dir-regexp 
      (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
``` 

This tells VC to ignore any path that looks like a TRAMP filename. As a result, opening a remote file won’t spawn `git status` or other VC checks in the background. **Doom Emacs users:** Doom sets `projectile` as the project manager which can also trigger VC-like scans; ensure you use a recent version of Projectile, as older versions had known TRAMP performance issues that have since been fixed ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=)). With VC/project scans disabled on remote buffers, you avoid a class of slowdowns (for instance, the mode line won’t constantly run `git` commands over TRAMP). You can still use Magit or manual Git commands on demand.

**6. Reduce TRAMP Verbosity (in normal use):** TRAMP’s debug logging can itself add slight overhead. In everyday use, you can lower `(setq tramp-verbose 1)` to minimize message output ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=%28setq%20remote,regexp)). The default is 3, which isn’t too high, but if you’re pushing for every drop of speed or noticing a lot of *tramp/ssh...* buffer spam, level 1 will only log errors. (Just remember to increase it temporarily when actively debugging – more on that later.) Lower verbosity might not make a night-and-day difference, but it shaves off a bit of Elisp processing for each operation.

**7. Other Helpful TRAMP Settings:** Ensure `(add-to-list 'tramp-remote-path 'tramp-own-remote-path)` is in your config. This makes TRAMP use the remote’s default `$PATH` (plus Emacs’ defaults) so it can find standard utilities. A correct remote PATH prevents delays where TRAMP might otherwise call `which` or fall back to hardcoded paths to locate commands like `ls`, `stat`, `git`, etc. Setting a sane remote shell and prompt is also important: by default Emacs tries to use a `/bin/sh` or `/bin/bash` with `ENVTERM=dumb` to get a simple prompt. If your remote host has a fancy shell prompt or initialization that confuses TRAMP, it can stall while “Waiting for remote prompts.” You can override the shell used via `tramp-login-shell` or connection local variables – e.g., force `/bin/bash` with no RC files. In Doom Emacs, one common tweak is in users’ configs: `(add-to-list 'tramp-connection-properties (list (regexp-quote "<host>") "remote-shell" "/bin/bash"))` to ensure a predictable shell. Also consider `(setq tramp-default-method "ssh")` (or "sshx") to avoid Tramp trying alternative methods. **In summary**, slim down TRAMP’s work: no extraneous backups/auto-saves (next section), no redundant lookups, and reuse the same connection and data wherever possible.

## Optimizing File Saving and Auto-Saves Over TRAMP 
Saving a file via TRAMP can be slow if Emacs tries to do things like create backup files or auto-save files on the remote system. Here are steps to streamline saves:

- **Store Auto-Saves Locally:** By default, Emacs might attempt to write the `#filename#` auto-save file in the same remote directory – meaning every auto-save interval, you’re writing to the remote. You can redirect these to a local directory. For example: `(setq tramp-auto-save-directory "~/emacs/tramp-autosave")` (or simply use the OS temp directory) ([Stop Emacs from reconnecting to SSH when working with a stale TRAMP buffer - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/73002/stop-emacs-from-reconnecting-to-ssh-when-working-with-a-stale-tramp-buffer#:~:text=%28setq%20tramp,autosave%22%29)). With this, auto-saves of remote files will be written locally, removing a frequent source of remote I/O latency. This prevents the editor “freezing” for seconds during auto-save in the middle of typing, which is a well-known annoyance ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=local%20one%2C%20but%20emacs%20experience,blocks%20for%20multiple%20seconds%2C%20etc)).

- **Disable or Customize Backups for Remote Files:** Similar to auto-save, making backups (the `~` files) on a remote host means copying the entire file over SSH before each save. If your workflow doesn’t require remote backups, you can turn them off for TRAMP. One method is to use `backup-directory-alist` to treat remote files specially. For example: 

  ```elisp
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ``` 

  This tells Emacs not to redirect remote file backups to any local directory (leaving `nil` means it would default to the remote location) ([Bizzarre Emacs Tramp Fix - Stack Overflow](https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix#:~:text=%3B%3B%20Backup%20%28file,verbose%2010)). In practice, many users simply disable backups globally (setting `make-backup-files` to nil) when working over TRAMP, or ensure backups go to a local cache. The goal is to avoid **additional file writes** over the network on each save. If you do want backups, consider setting `backup-by-copying` and a local `backup-directory-alist` path, so Emacs transfers the file once and saves the backup locally.

- **Avoid File Locking Delays:** Emacs creates lockfiles (`.#filename`) to warn if two Emacs instances edit the same file. Over TRAMP, lockfiles can add overhead (an extra file existence check and creation). If you’re the sole editor, you might disable lockfiles: `(setq create-lockfiles nil)`. This is a minor gain, but it’s one less remote write to worry about. 

By handling backups/auto-save/locks as above, saving changes to a remote file should boil down to a single write operation (sending the buffer contents) plus perhaps a `chmod` or `touch` for timestamps. The improvement can be dramatic – users have observed multi-second save times drop to near-instant by eliminating auto-save and VC-induced delays ([Bizzarre Emacs Tramp Fix - Stack Overflow](https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix#:~:text=Well%20tramp,freeze)). In fact, auto-save and VC checks are cited as “two of the biggest culprits” for TRAMP latency, especially on slower connections ([Bizzarre Emacs Tramp Fix - Stack Overflow](https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix#:~:text=Well%20tramp,freeze)). After these tweaks, you’ll notice that typing doesn’t hitch every few seconds, and `C-x C-s` returns control to you much faster.

## Faster Dired and Directory Navigation 
Browsing directories with `C-x d` (Dired) over TRAMP can be slow if Emacs performs extra work per listing. Here’s how to speed it up:

- **Disable Dired Enhancements that Scan Remote Repos:** If you use Doom Emacs or have packages like *diff-hl* or *dired-git-info*, be aware these can invoke version control commands in each Dired buffer. Doom Emacs enables file diff highlighting in Dired, which runs `git diff` under the hood. On a remote Git repo, that means for each directory entered, TRAMP may spawn a new `git` process and even a new SSH connection repeatedly ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=What%20actually%20happened%3F%20Once%20you,buffer)) ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=Additional%20details%3A%20This%20seems%20to,the%20connection%20and%20no%20reconnects)). This was the cause of a Doom issue where entering subdirectories of a remote Git project felt extremely slow. The solution (now implemented in Doom) was to **disable `diff-hl-dired-mode` for remote buffers** ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=mihai,74)). Doom’s config uses `(diff-hl-dired-mode-unless-remote)` to only enable diff-hl in local Dired buffers. If you aren’t on a recent Doom version, consider manually disabling `diff-hl` or similar VC integrations when using TRAMP. The difference is night and day – in tests, turning off remote diff-hl stopped TRAMP from re-opening an SSH connection for every directory change ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=mihai,74)). 

- **Turn off `ls` quoting quirks if needed:** TRAMP tries to parse `ls -l` output. If your remote `ls` is aliased to add color or strange formatting, it can confuse or slow down Dired. Ensure that your remote shell for TRAMP is non-interactive (no aliases) – usually TRAMP sets `ENV=TERM=dumb` and `LC_ALL=C` for commands. If Dired listings are still slow or hang, explicitly setting `(setq tramp-default-remote-shell "/bin/sh")` or similar can help. Another tip: you can try `(setq tramp-use-ssh-controlmaster-options nil)` if you suspect TRAMP’s internal SSH wrapper is misbehaving, and rely on your own SSH config for multiplexing – but normally this isn’t needed unless debugging a TRAMP bug.

- **Limit Large Directory Listings:** If you are dealing with directories containing thousands of files, Emacs will be slow regardless (since it has to process that list). TRAMP doesn’t yet support a paging mechanism for Dired. As a workaround for extremely large directories, consider using command-line SSH (`M-x shell` or `M-x term`) to manually list and narrow what you open in Emacs. This is more of a tip to avoid edge-case slowness rather than a setting.

In short, keep Dired simple on remote hosts. Disable fancy VCS integration and let it do basic `ls` listing only. Emacs will still cache the directory contents for you (honoring `remote-file-name-inhibit-cache` as mentioned), so revisiting an already-opened directory within the cache window is quick. With these adjustments, navigation in remote Dired should feel closer to local navigation – only a slight initial delay for the listing, not a per-entry slog.

## Magit and Git on Remote Repositories 
Using Magit on a TRAMP path is convenient but can be slow, because Magit runs many Git commands which TRAMP must execute over SSH. Every Magit action (status refresh, diff, stage, commit) may trigger multiple child processes. Here’s how to improve the experience:

- **Ensure Connection Reuse for Git Processes:** TRAMP normally reuses the main SSH connection for subprocesses, but there have been scenarios where `process-file` calls (like those Magit uses) opened separate connections. For instance, users noticed that `M-x magit-status` on a `/ssh:host:/repo` would output “Opening connection for **git** for host using ssh” repeatedly, instead of reusing the existing “nil for host” connection ([magit - Tramp not reusing connections for Git commands - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/80662/tramp-not-reusing-connections-for-git-commands#:~:text=Tramp%3A%20Opening%20connection%20git%20for,using%20ssh)) ([magit - Tramp not reusing connections for Git commands - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/80662/tramp-not-reusing-connections-for-git-commands#:~:text=What%20is%20strange%20is%20that,git)). This indicates TRAMP treated the Git calls differently (possibly a bug or limitation). To force reuse, make sure you applied the earlier advice about `tramp-use-connection-share` (so TRAMP is allowed to multiplex) and that your SSH config’s ControlMaster is on. In cases where TRAMP *still* spawns a new connection for each Git command, a workaround is to **disable TRAMP’s SSH option handling**: set `(setq tramp-use-ssh-controlmaster-options nil)`. This will cause TRAMP to invoke your system’s `ssh` without `-o ControlPersist=no`, so if your SSH config has `ControlPersist yes`, all those git processes should attach to the same persistent master. In practice, this can eliminate repeated handshakes for each Magit sub-process. (Emacs 30’s TRAMP might have improvements here; TRAMP’s maintainer Michael Albinus provided fixes around connection sharing for subprocesses – e.g., Emacs 30 now unifies the control of connection sharing for all methods ([What's New in Emacs 30.1? - Mastering Emacs ](https://www.masteringemacs.org/article/whats-new-in-emacs-301#:~:text=match%20at%20L1819%20Rename%20%27tramp,based%20and)).)

- **Use Magit cautiously on huge repos:** Even with perfect connection reuse, running `magit-status` on a very large remote repository will be slower than locally. Magit might call `git diff --name-status` on dozens of files, or `git ls-files` etc., each incurring network latency. Some users opt to run Magit on a local clone of the repo if available (using Magit’s ability to push/pull to remote), and only use TRAMP for editing single files that must be on the remote. If that’s feasible, it sidesteps the issue. If not, just be aware that operations like staging 1000 files will cause 1000+ round-trips (Magit is fast, but it’s not batching all those operations into one remote call).

- **Consider `magit-remote-git-executable`:** There is a Magit variable `magit-git-executable` which normally is just "git". In a TRAMP context, Magit will call the Git available on the remote (via TRAMP). Ensure that the Git on the remote is reasonably up-to-date and not doing something odd (like waiting on credential input). Usually not a problem, but it’s worth checking if you experience weird stalls. 

In summary, Magit’s slowness over TRAMP mostly boils down to TRAMP overhead, not Magit itself. By maximizing connection sharing (no repeated SSH logins for each command) and limiting the scope of Magit operations to what you need, you can make it *usable*. It may never be as snappy as Magit on a local repo – that’s an Emacs limitation – but it should be far from “unusably slow.” Keep an eye on the TRAMP connection indicator in the mode line or *Messages*; if you see reconnections or new [tramp] buffers piling up for each Magit action, that’s a sign the sharing isn’t working and you should adjust the above settings. As one Stack Overflow answer noted, “these days Tramp will try to use ControlMaster by default with ssh methods in order to re-use an open connection” ([emacs - magit over tramp: re-use ssh connection - Stack Overflow](https://stackoverflow.com/questions/56105716/magit-over-tramp-re-use-ssh-connection#:~:text=Which%20OS%20and%20which%20ssh,when%20Windows%2BCygwin%20couldn%27t%20do%20it)) – leverage that capability to avoid repetitive SSH handshakes during Magit use.

## Doom Emacs Considerations 
Doom Emacs is a configuration framework that bundles many packages, and while it doesn’t fundamentally change TRAMP, it has some defaults that can affect remote editing. According to Doom’s maintainer, Doom “configures TRAMP quite minimally” ([Slow load time over ssh using tramp/dired/opening files · Issue #3909 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/3909#:~:text=hlissner%20%20%20commented%20,80)) – meaning it doesn’t override core TRAMP settings beyond perhaps the default method or path – so the optimizations above (which are mostly Emacs settings) apply equally under Doom. However, **Doom users should be aware of a few module interactions:**

- **Diff-hl and Dired in Doom:** As discussed, Doom had an issue where `diff-hl` (enabled by Doom’s `:emacs dired` module) caused repeated reconnections ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=What%20actually%20happened%3F%20Once%20you,buffer)) ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=mihai,74)). This has been fixed by Doom (the config now avoids enabling diff-hl in remote Dired buffers), but if you’re on an older release or have a custom Doom config, ensure that fix is in place. In Doom’s `dired/config.el`, the hook uses `diff-hl-dired-mode-unless-remote` – mimic that in your config if needed ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=I%20assume%20the%20problem%20starts,emacs%2Fblob%2Fdevelop%2Fmodules%2Femacs%2Fdired%2Fconfig.el%23L68.%20It%20should%20be)). The result is that opening a remote directory in Dired will no longer trigger `git diff` calls that slowed things down.

- **Project Management (Projectile):** Doom includes Projectile by default. Historically, Projectile would try to index a project’s files, which over TRAMP was disastrous. Newer Projectile is smarter and checks `file-remote-p` to avoid scanning remote projects. Just make sure you’re on a recent Doom (which includes an updated Projectile) – the issue was well known and patched ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=)). If you still encounter something like `Projectile: caching files...` when you open a remote project, you can manually disable Projectile in that session (`M-x projectile-mode`) or increase `projectile-file-exists-local-cache-expire`. But ideally, it shouldn’t run at all on TRAMP paths now.

- **Completions and Fuzzy Finders:** Doom might use ido/vertico or others for minibuffer completion. If your current buffer is remote, some completion frameworks might inadvertently scan remote directories (for file name completion, etc.). For example, older Helm/Ivy setups sometimes invoked `find` or similar. In Doom’s default Vertico + consult setup, this is less of an issue, but be cautious with any config that does auto-completion of file names or paths while you type – if you notice lag in the minibuffer, check if it’s hitting TRAMP. You might need to disable certain sources or increase `completion-auto-help` delay when on remote.

- **Terminal/Shell in Doom:** Using `M-x shell` or `M-x term` in a TRAMP buffer actually spawns a remote shell through TRAMP. Doom doesn’t alter that, so the same latency issues apply. If you use `M-x vterm` (if you have the vterm module), note that **vterm will run a local shell, not remote** (unless you ssh manually inside it). So for running commands on the remote, a TRAMP `*shell*` might feel slow (each command’s output is subject to TRAMP delays). This is expected, not a Doom bug – just something to keep in mind (i.e., running a compile job or grep via TRAMP shell might be slow; an alternative is to use `ssh` in a local vterm which bypasses TRAMP for that use-case).

In conclusion, Doom Emacs users should apply all the Emacs optimizations described earlier. There’s nothing Doom-specific to *enable* for TRAMP performance, but you might need to **disable** or tweak a few Doom features as noted. The Doom community forums and docs have additional tips; for instance, one user’s config shared on Reddit included the same key settings (disabling VC, enabling cache, etc.) we’ve covered ([Stop Emacs from reconnecting to SSH when working with a stale TRAMP buffer - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/73002/stop-emacs-from-reconnecting-to-ssh-when-working-with-a-stale-tramp-buffer#:~:text=%28setq%20tramp,cache%20nil)) ([Stop Emacs from reconnecting to SSH when working with a stale TRAMP buffer - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/73002/stop-emacs-from-reconnecting-to-ssh-when-working-with-a-stale-tramp-buffer#:~:text=%28setq%20tramp,autosave%22%29)). So the consensus is that Doom doesn’t impede TRAMP if configured properly. After applying these tweaks, using TRAMP within Doom Emacs should be as efficient as vanilla Emacs TRAMP can get.

## Debugging and Profiling TRAMP Slowdowns 
Despite best efforts, you might still run into TRAMP being slower than expected. In such cases, it’s useful to **profile and debug** what TRAMP is doing:

- **Increase `tramp-verbose` for Diagnostics:** Set `(setq tramp-verbose 6)` (or even 7–10 for extremely detailed logs) temporarily ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=Tramp%20has%20extended%20logging%20facilities,version)). This will cause TRAMP to log each step of its communication with the remote, with timestamps. Reproduce the slow operation, then check the `*tramp/ssh ...*` buffer (or *Messages*). Look for gaps in timestamps to identify which command is slow. For example, you might see TRAMP sending an `ls` command and taking several seconds before output – indicating the remote responded slowly, or perhaps a stuck prompt. Or you might notice TRAMP repeatedly opening new connections (lines with “Opening connection for ...”). The verbose log is extremely detailed (level 6 shows every command/result, level 10 even more). This is the single best tool to pinpoint where time is going. As one Emacs developer suggested, “There will be an exhaustive Tramp debug buffer, every single entry starts with a timestamp… Check whether there are obvious delays” ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=Tramp%20has%20extended%20logging%20facilities,version)). This can reveal, for instance, that a particular shell command is timing out or a pattern in your prompt is confusing TRAMP’s parser.

- **Use Emacs Profiler:** Emacs has a built-in profiler. Run `M-x profiler-start CPU` (and/or Memory), perform some TRAMP actions (e.g. open a file, save, refresh Dired), then `M-x profiler-report`. This will show which Emacs Lisp functions consumed the most time. You might see functions like `tramp-sh-handle-expand-file-name` or `tramp-sh-handle-file-exists-p` dominating, which indicates TRAMP is spending a lot of time on file name translations or existence checks. Or you might see a non-TRAMP function (e.g. an auto-save hook or a mode line function) taking time – which could hint that some other package is misbehaving on remote files. The profiler won’t directly show the latency of external processes (that time will just show up as part of tramp functions), but it helps separate “TRAMP overhead” from “other Emacs overhead triggered by TRAMP.”

- **Benchmark Specific Actions:** For granular timing, you can use `M-x benchmark-run` or write a small Emacs Lisp snippet. For instance: 

  ```elisp
  (benchmark-run 1 (insert-file-contents "/ssh:host:/path/to/file"))
  ``` 

  to time a file read, or use `(current-time)` before and after an operation. This can be useful to compare improvements as you tweak settings. It’s less necessary than the above methods, but useful if you want to quantitatively measure (say, before/after enabling ControlMaster).

- **Network-level Diagnostics:** If you suspect a network issue, you can run SSH in verbose mode outside Emacs to see if there’s any obvious lag (e.g., DNS lookup delays, authentication retries). Inside Emacs, you could even attach a debugger like `strace -p <emacs-pid>` to see system calls, but that’s overkill. Usually, `tramp-verbose` output is sufficient to track the slowness.

- **Cleaning Up and Resetting:** TRAMP caches a lot (for good reason), but sometimes you might want to clear state between tests. Use `M-x tramp-cleanup-all-connections` to close all SSH sessions and flush caches. Also `M-x tramp-cleanup-all-buffers` to kill any lingering remote buffers. This ensures each test is from a clean slate. If you find that after hours of editing, TRAMP gets slower or behaves oddly, doing a cleanup can sometimes restore performance (for example, if a connection hung or a cache grew stale).

Using these tools, you can identify if a particular setting is effective. For example, you might test directory listing time with `tramp-verbose` 6: if you see 5 successive “stat” commands taking 0.2s each, you know caching isn’t working and you might revisit `remote-file-name-inhibit-cache`. Or if you see a long pause before a prompt appears, maybe your `~/.bashrc` on the remote is printing a banner or running a slow command – which you can then disable for non-interactive shells. In essence, treat TRAMP like any other program: gather data with logs and profiler, adjust settings, and repeat.

## Known Pitfalls and Bugs (Emacs 30.1, TRAMP, Doom) 
Emacs 30.1 is relatively up-to-date, but it’s worth noting a few quirks:

- **“git:” Multi-hop Bug (Fixed in Emacs 30):** Earlier Emacs versions had a quirk where running certain commands (like Git via Magit) over TRAMP would appear as a separate “method” in the TRAMP connection (shown as `Opening connection git for host`). This was essentially a bug – TRAMP wasn’t reusing the existing connection for processes invoked via `start-file-process`. Michael Albinus addressed this in TRAMP updates. As of Emacs 30, the unification of `tramp-use-connection-share` should make TRAMP better at sharing the link for such subprocesses ([What's New in Emacs 30.1? - Mastering Emacs ](https://www.masteringemacs.org/article/whats-new-in-emacs-301#:~:text=match%20at%20L1819%20Rename%20%27tramp,based%20and)). So if you’re on 30.1 and fully updated, you benefit from those fixes. If you still see “Opening connection git for …” messages, ensure you have the latest patch release of Emacs 30, as this might indicate an update is needed or a configuration forcing a new connection.

- **Doom Emacs `:remote` Module:** Doom has an optional module `:tools uploader` and others for remote interactions, but nothing that directly changes TRAMP. There was a community module for TRAMP in Doom at one point, but it’s not in core. The main Doom-specific bug was the diff-hl issue we covered. Another one was an old issue with `doom-project-find-file` interacting poorly with TRAMP (#2179) – but that was more about Doom’s project detection, not performance. It’s been resolved or made moot by changes in Projectile and Doom.

- **Emacs 29/30 File-Name Handling Changes:** Emacs 29 introduced some refactoring in file name handlers (which include TRAMP). If you wrote any custom Lisp that touches remote files, be aware some APIs changed. One notable variable is `file-name-handler-alist` now being temporarily let-bound to nil in more situations for performance. This doesn’t usually affect TRAMP unless you do something very advanced, but if you see an error about “forbidden reentrant call of Tramp” (as seen in some Eglot issues ([Forbidden reentrant call of Tramp #859 - joaotavora/eglot - GitHub](https://github.com/joaotavora/eglot/issues/859#:~:text=Forbidden%20reentrant%20call%20of%20Tramp,version%20of%20Emacs%20being%20used))), it may be due to a recursive invocation of a TRAMP operation. Simply avoid calling TRAMP functions reentrantly (e.g., don’t trigger a TRAMP find-file in the middle of another TRAMP process callback).

- **Unattended SSH Auth Prompts:** If you use methods like `ssh` with keys or agent, TRAMP is seamless. But if it ever falls back to password and you’re not around to notice the prompt, Emacs might appear frozen. This isn’t a performance issue per se, but it can *look* like one. Always ensure your SSH can connect non-interactively (use SSH keys or ensure `ssh-agent`/Pageant is running) for best results. Doom’s documentation also recommends this: TRAMP works best when it doesn’t have to ask for credentials in the middle of a command.

In general, check Emacs’s bug tracker and Doom’s issue tracker if you suspect a genuine bug. The combination of Emacs 30 + TRAMP is fairly mature. Many older bugs (like TRAMP hanging if an `scp` process died, or issues with remote shell prompts) have been fixed in recent years. The remaining performance “issues” are usually just the inherent latency, not something that a code fix can eliminate.

## Inherent Limitations and Alternatives 
After applying all these optimizations, TRAMP should be **usable and reasonably fast** for most day-to-day tasks. However, it’s important to set realistic expectations: editing over a high-latency network will never be as instantaneous as local editing. Emacs/TRAMP has a fundamentally different model than, say, SSHFS or remote-IDE setups. As one user bluntly noted, even with optimizations, “everything that requires remote file system interaction is super slow... Emacs just seems to be doing lots of individual I/O operations” ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=I%20have%20this%2C%20yet%20I,and%20blocking%20on%20their%20completion)). TRAMP operates synchronously on the Emacs main thread, so any network hiccup directly impacts editor responsiveness ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=The%20other%20thing%20that%20makes,blocks%20for%20multiple%20seconds%2C%20etc)). Complex operations (searching across files, recursive grep, large refactors) will compound those delays.

**When is slowness unavoidable?** 
- If you have very high latency (e.g. connecting across continents on a slow VPN), each file stat or open might take 100+ milliseconds. Even 10 such ops will introduce a noticeable pause. TRAMP can’t magically eliminate network latency.
- Operations that inherently require many calls – e.g., computing a directory’s size by checking every file, or Magit diff across hundreds of files – will still be bounded by the number of round-trips.
- Emacs itself, being single-threaded, means it can’t parallelize remote operations. It waits for one to finish before starting the next. So throughput is one-at-a-time.

**What can you do in such cases?** If you hit a wall where TRAMP is too slow for a certain task, consider these alternatives as a workaround:
- **Running Emacs on the remote server:** If you have the option, running a GUI Emacs via X11 forwarding or a terminal Emacs over SSH (with Mosh or similar) can sometimes be more responsive for heavy operations. Then Emacs is local to the files, and you only send keystrokes and screen updates over the network (which for some workflows is less data than file contents). The downside is you need Emacs and your config on the server, and maybe an X server if you want GUI.
- **Using SSHFS / External Mounts:** Mounting the remote filesystem on your local machine (via SSHFS or SFTP NetDrive on Windows, etc.) lets Emacs treat it as local files. This can be faster for bulk operations because the kernel might handle caching and parallel reads better than TRAMP. However, Emacs over SSHFS has its own quirks (e.g., file notification may not work, and you might still get some stalls if the filesystem blocks on network).
- **Mix TRAMP with Local Clones:** For development in large Git repositories, a hybrid approach works: use TRAMP for quick edits on remote-only files or config changes, but for coding in a big project, keep a local clone in sync. You can use Magit locally and `git push` to the remote, or use `M-x compile` to run remote builds over SSH while editing locally. This way you aren’t constantly invoking TRAMP for every file operation. It’s not *TRAMP* per se, but it leverages TRAMP for the few cases you need (like editing a config file on a server) and avoids it when it would be painful (like searching across a codebase).

- **Accept some delays for certain actions:** Finally, sometimes the simplest answer is patience. If you’ve tuned everything and a `C-x C-f` to open a new remote file still takes 2 seconds, that might just be the physics of your network. The goal is to reduce the _unnecessary_ waits. A 100ms baseline delay might be unavoidable, but 10s delays due to misconfiguration are not. We’ve focused on eliminating the latter.

Remember, many Emacs users successfully do remote development with TRAMP every day – it *can* be made to work smoothly for editing, navigating, and light repository operations. But for truly latency-sensitive work, you have to consider the above alternatives. TRAMP’s design favors simplicity and compatibility (it works over plain SSH and requires no additional remote software), at the cost of some performance. Emacs 30.1 continues to improve TRAMP (e.g., adding container integration, better multi-hop completion, etc.), but until Emacs gets more asynchronous or multi-threaded capabilities for file I/O, there will always be a gap between TRAMP and solutions that run a dedicated agent on the remote side.

## Conclusion 
By applying the optimizations in this guide, you should see a significant improvement in TRAMP’s responsiveness for everyday tasks. We addressed the common bottlenecks: ensuring one persistent SSH connection (not dozens of re-logins), avoiding unnecessary work like backup files and VC checks on each save, and configuring Doom Emacs and other packages to not sabotage our efforts. We also discussed how to diagnose remaining issues with TRAMP’s verbose logs and Emacs profiling, and set expectations about the limits of TRAMP’s performance. 

In practice, a well-tuned Emacs TRAMP (especially with Emacs 30+ and a solid network) can feel quite seamless – opening and saving files with only slight delays, and even using Magit or Dired on remote hosts reasonably. Official documentation and community tips align with what we’ve compiled here: **cache aggressively**, **cut out extra overhead**, **use SSH features**, and **be mindful of modes that invoke extra commands**. With these in place, Emacs + TRAMP + Doom can be a powerful remote editing setup that lets you leverage all your local customizations on distant machines with minimal frustration.

#### Sources

- GNU Emacs Manual – *TRAMP FAQ (Performance)* ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=I%20have%20also%20found%20the,up%20to%20work%20pretty%20well))  
- Doom Emacs Issue Tracker – *Remote Dired slow due to diff-hl* ([Tramp session gets re-established over and over when traversing git repo directory structure · Issue #2439 · doomemacs/doomemacs · GitHub](https://github.com/hlissner/doom-emacs/issues/2439#:~:text=mihai,74))  
- Stack Overflow – *“Bizarre Emacs TRAMP Fix” (auto-save and vc interference)* ([Bizzarre Emacs Tramp Fix - Stack Overflow](https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix#:~:text=Well%20tramp,freeze)) ([Bizzarre Emacs Tramp Fix - Stack Overflow](https://stackoverflow.com/questions/13997965/bizzarre-emacs-tramp-fix#:~:text=%28add,verbose%2010))  
- Stack Exchange (Emacs) – *“TRAMP not reusing connections for Git commands”* ([magit - Tramp not reusing connections for Git commands - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/80662/tramp-not-reusing-connections-for-git-commands#:~:text=Tramp%3A%20Opening%20connection%20git%20for,using%20ssh)) ([magit - Tramp not reusing connections for Git commands - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/80662/tramp-not-reusing-connections-for-git-commands#:~:text=What%20is%20strange%20is%20that,git))  
- Hacker News – *Discussion of Emacs vs VSCode remote architecture* ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=The%20other%20thing%20that%20makes,blocks%20for%20multiple%20seconds%2C%20etc)) ([I think nativcomp doesn't really improve what makes emacs "slow". Emacs is a sin... | Hacker News](https://news.ycombinator.com/item?id=27012799#:~:text=I%20have%20this%2C%20yet%20I,and%20blocking%20on%20their%20completion))  
- Mastering Emacs – *What’s New in Emacs 30.1 (TRAMP connection sharing)* ([What's New in Emacs 30.1? - Mastering Emacs ](https://www.masteringemacs.org/article/whats-new-in-emacs-301#:~:text=match%20at%20L1819%20Rename%20%27tramp,based%20and))  
- Emacs Stack Exchange – *TRAMP slower than ssh and solutions* ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=I%20have%20also%20found%20the,up%20to%20work%20pretty%20well)) ([shell - Tramp mode is much slower than using terminal to ssh - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh#:~:text=Tramp%20has%20extended%20logging%20facilities,version))  
- Doom Emacs Discussions – *General TRAMP tips and config (community)* ([Stop Emacs from reconnecting to SSH when working with a stale TRAMP buffer - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/73002/stop-emacs-from-reconnecting-to-ssh-when-working-with-a-stale-tramp-buffer#:~:text=%28setq%20tramp,cache%20nil))

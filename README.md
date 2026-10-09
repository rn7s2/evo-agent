# evo

`evo` is a goal-oriented, self-evolving software agent written in Common
Lisp. Give it an objective, and it works in your terminal until the job is done.
When it needs a tool it doesn't have, it writes one into its own running image
and keeps going.

Five properties define the system:

- **Goal-oriented.** You state what *done* means; evo decides how to get there.
  Long-running, unattended work is the normal case.
- **Self-extending.** Evo writes Lisp and loads it into its own runtime to add
  tools and capabilities without restarting.
- **Permissive.** No permission prompts. Your OS or container is the trust
  boundary; the kernel/userspace split guards against accidental changes to
  the core.
- **Self-healing.** Crash, restart, resume, continue the goal. The supervisor
  and journal make process death recoverable.
- **Minimal.** A small kernel owns the agent loop. The interface, MCP and other
  capabilities live in extensions; parallel agents live in `evo-swarm`.

`evo` runs a coordinator with worker agents: it splits the work, delegates,
and checks the results. Use `evo-agent` for a single agent.

## Quick start

Follow the [Common Lisp getting-started guide](https://lisp-lang.org/learn/getting-started/)
to install **SBCL and Quicklisp**. Then, from this repository, run:

```sh
make install
```

Enter your password if prompted. Installation is complete. Start evo in your
project directory:

```sh
evo
```

### AI provider

Have a Claude or OpenAI subscription? Run the matching slash command inside evo
and follow the browser login:

- **Claude:** `/claude-oauth:login`
- **OpenAI (ChatGPT):** `/openai-oauth:login`

After your first Claude login, run `/reload`. Then use `/model` to pick a model,
and you're ready. For API keys, custom endpoints or other models, see the
[configuration reference](docs/extension-api.md#configuration-initlisp).

### Optional: default model, thinking and workers

After logging in, save this in `~/.evo/init.lisp` to choose the default model
and thinking level for `evo-agent` and `evo`:

```lisp
(evo:set-setting :model "claude-opus-5-5")
(evo:set-setting :thinking :high)
```

For OpenAI, use `"gpt-6.1-sol"` instead. Use `/model` to see available models.

For `evo` / `evo-swarm`, save this in `~/.evo/swarm.lisp`:

```lisp
(evo:set-setting :swarm-workers 4)
(evo.swarm:in-lanes ()
  (load "~/.evo/extensions/020-claude-oauth-provider.lisp")
  (load "~/.evo/extensions/020-openai-oauth-provider.lisp")
  (evo:set-setting :model "claude-sonnet-5-5")
  (evo:set-setting :thinking :medium))
```

The coordinator uses your defaults; this example gives workers Sonnet with
medium thinking. For OpenAI workers, use `"gpt-6.1-sol"` instead. Omit the
worker model and thinking settings to inherit the coordinator's. The loads
let workers use your subscription logins.

## Desktop app

Prefer a GUI? [evo-gui (Evo Desktop)](https://github.com/rn7s2/evo-gui.git) is a
native macOS app with a tab per project and a live view of the coordinator and
its workers.

For the architecture and technical references, see [design.md](design.md) and
[docs/](docs/).

## License

[MIT](LICENSE).

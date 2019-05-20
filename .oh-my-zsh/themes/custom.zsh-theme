local fg_blue=%{$fg_bold[blue]%}
local fg_cyan=%{$fg_bold[cyan]%}
local fg_green=%{$fg_bold[green]%}
local fg_red=%{$fg_bold[red]%}
local fg_yellow=%{$fg[yellow]%}
local fg_reset=%{$reset_color%}

local ret_status="%(?..${fg_red}! )"

local current_dir=${fg_cyan}%(6~.%5/.%~)
local prompt_symbol=%(!.${fg_red}#.${fg_blue}$)

PROMPT='${ret_status}${current_dir} $(git_prompt_info)${prompt_symbol} ${fg_reset}'

ZSH_THEME_GIT_PROMPT_PREFIX="${fg_blue}git:(${fg_red}"
ZSH_THEME_GIT_PROMPT_SUFFIX="${fg_reset}"
ZSH_THEME_GIT_PROMPT_DIRTY="${fg_yellow}*${fg_blue}) "
ZSH_THEME_GIT_PROMPT_CLEAN="${fg_blue}) "

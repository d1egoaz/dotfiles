# Shared LLM provider configuration by machine profile.
# Keep app modules agnostic: they should consume only a resolved llmConfig.
let
  profiles = {
    personal = {
      provider = "Cerebras";
      model = "gpt-oss-120b";
      base_url = "https://api.cerebras.ai/v1";
      key_item = "Cerebras API";
    };

    office = {
      provider = "OpenAI";
      model = "gpt-5-mini";
      base_url = "https://api.openai.com/v1";
      key_item = "OpenAI API";
    };
  };
in
profiles
// {
  forProfile = profile: profiles.${profile} or profiles.personal;
}

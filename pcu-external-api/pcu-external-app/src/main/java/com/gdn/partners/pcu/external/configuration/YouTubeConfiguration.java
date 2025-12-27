package com.gdn.partners.pcu.external.configuration;

import com.google.api.client.json.jackson.JacksonFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.services.youtube.YouTube;

/**
 * @author Parvej
 */
@Configuration
public class YouTubeConfiguration {

  private static final String APP_ID = "pcu_external_api";

  @Bean
  public static YouTube youTube() {
    return new YouTube.Builder(new NetHttpTransport(), new JacksonFactory(), request -> {
    }).setApplicationName(APP_ID).build();
  }
}

package com.gdn.mta.bulk.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShortUrlMappingRequest {

  private String name;
  private boolean active;
  private String longUrl;
  private List<UrlMapping> urlMappingUtmList;
}

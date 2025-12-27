package com.gdn.partners.pcu.master.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DimensionWebRequest {
  private String name;
  private String nameEnglish;
  private byte[] description;
  private byte[] descriptionEnglish;
  private String example;
  private String dsAttributeName;
}
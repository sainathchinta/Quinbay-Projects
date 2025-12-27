package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageFaultyTypeWebResponse {

  private String enName;
  private String inName;
  private Boolean ignoreForImageQc;
  private String labelColour;

  public ImageFaultyTypeWebResponse(String enName, String inName) {
    this.enName = enName;
    this.inName = inName;
  }

  public ImageFaultyTypeWebResponse(String enName, String inName, Boolean ignoreForImageQc) {
    this.enName = enName;
    this.inName = inName;
    this.ignoreForImageQc = ignoreForImageQc;
  }
}

package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpcCodeAndImagesImageWebResponse implements Serializable {
  private static final long serialVersionUID = 7342332623879487749L;

  private boolean isMainImage;
  private String locationPath;
  private int sequence;
}

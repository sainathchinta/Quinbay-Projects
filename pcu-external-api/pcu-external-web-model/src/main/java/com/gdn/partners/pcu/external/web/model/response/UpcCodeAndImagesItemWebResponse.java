package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.List;

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
public class UpcCodeAndImagesItemWebResponse implements Serializable {
  private static final long serialVersionUID = -1646986046317591185L;

  private String itemSku;
  private String upcCode;
  private List<UpcCodeAndImagesImageWebResponse> images;
}

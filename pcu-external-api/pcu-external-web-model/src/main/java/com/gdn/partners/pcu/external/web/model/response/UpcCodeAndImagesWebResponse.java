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
public class UpcCodeAndImagesWebResponse implements Serializable {
  private static final long serialVersionUID = -8703517983018055212L;
  private String productSku;
  private List<UpcCodeAndImagesItemWebResponse> itemResponses;
}

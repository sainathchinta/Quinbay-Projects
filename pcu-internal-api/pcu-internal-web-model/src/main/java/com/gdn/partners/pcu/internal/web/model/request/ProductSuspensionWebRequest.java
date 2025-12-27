package com.gdn.partners.pcu.internal.web.model.request;

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
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ProductSuspensionWebRequest implements Serializable {

  private static final long serialVersionUID = -7440155694624385802L;

  private String productSku;
  private String productName;
  private String merchantCode;
}

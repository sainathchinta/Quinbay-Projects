package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AppealProductWebRequest {
  private String notes;
  @NotBlank(message = "Product Code should not be blank")
  private String productCode;
  @NotBlank(message = "Product Sku should not be blank")
  private String productSku;
}

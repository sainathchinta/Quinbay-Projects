package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

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
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class HalalProductWebResponse {
  private String productSku;
  private String productCode;
  private String brand;
  private Date productCreationDate;
  private boolean halalProduct;
  private String curationStatus;
  private String productLink;
  private String productName;
  private String certificateNumber;

}

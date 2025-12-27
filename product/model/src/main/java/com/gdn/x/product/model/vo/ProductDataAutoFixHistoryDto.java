package com.gdn.x.product.model.vo;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductDataAutoFixHistoryDto {
  private String productCode;
  private String type;
  private String additionalInfo;
}

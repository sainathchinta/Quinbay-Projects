package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ValidateDuplicateProductRequest implements Serializable {
  private static final long serialVersionUID = -7973988726228004585L;
  private String keyword;
  private boolean byProductName;
  private boolean bySellerSku;
}

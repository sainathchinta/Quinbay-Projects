package com.gda.mta.product.dto;

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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class EditFlagChangesDTO {
  private boolean isB2bFlagChangedAtL3Level;
  private boolean isB2cFlagChangedAtL3Level;
  private boolean takeActionOnShippingForAutoCategoryChange;

}

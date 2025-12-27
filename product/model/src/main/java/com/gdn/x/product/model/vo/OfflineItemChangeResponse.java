package com.gdn.x.product.model.vo;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemChangeResponse {
  private boolean isCncChanged;
  private boolean isPriceChanged;
  private boolean ifMFDChanged;
  private boolean isViewConfigChanged;
  private boolean isDiscoverableChanged;
  private boolean isCncDiscoverableChanged;
  private boolean isNew;
}

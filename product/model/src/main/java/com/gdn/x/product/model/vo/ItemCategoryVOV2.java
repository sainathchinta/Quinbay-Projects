package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCategoryVOV2 extends ItemCategoryVO implements Serializable {
  private boolean categoryActive;
  private boolean display;
}

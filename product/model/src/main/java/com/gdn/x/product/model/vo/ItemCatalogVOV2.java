package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCatalogVOV2 implements Serializable {
  private static final long serialVersionUID = 1L;
  private String catalogId;
  private List<ItemCategoryVOV2> itemCategories;
}

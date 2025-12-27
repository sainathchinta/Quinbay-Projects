package com.gdn.x.productcategorybase.dto;

import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryProductAttributeExtractedDTO {
  private Category category;
  private ProductAttributeExtracted productAttributeExtracted;
}

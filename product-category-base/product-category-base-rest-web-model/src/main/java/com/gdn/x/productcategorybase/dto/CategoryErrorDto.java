package com.gdn.x.productcategorybase.dto;

import com.gdn.x.productcategorybase.entity.Category;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class CategoryErrorDto {
  private Category category;
  private String errorCode;
  private String errorMessage;
}

package com.gdn.partners.pbp.entity.mailEvent;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CategoryChangeMailEvent {
  private String newCategoryName;
  private String existingCategoryName;
  private String newCategoryCode;
  private String existingCategoryCode;
  private String newCategoryMargin;
  private String oldCategoryMargin;
  private String newCategoryValueType;
  private String oldCategoryValueType;
}

package com.gdn.partners.pcu.master.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryMappingResponse {
  private String id;
  private String name;
  private String nameEnglish;
  private String categoryCode;
  private boolean activated;
  private boolean mapped;
  private long childCount;
  private String parentCategoryId;
  private List<CategoryMappingResponse> children;
}

package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryHierarchyDataResponse implements Serializable {
  private static final long serialVersionUID = -668805463827119701L;
  private String categoryCode;
  List<CategoryDataResponse> itemCategories = new ArrayList<>();
}

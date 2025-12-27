package com.gdn.partners.pcu.master.client.model;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class SuggestedCategoriesResponse {
  private List<CategorySuggestionResponse> categories;
  private double score;
}

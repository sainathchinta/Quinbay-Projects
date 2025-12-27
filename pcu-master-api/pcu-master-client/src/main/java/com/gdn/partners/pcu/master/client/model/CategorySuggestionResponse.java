package com.gdn.partners.pcu.master.client.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class CategorySuggestionResponse {
  private String category_level;
  private String category_id;
  private String category_code;
  private String category_name;
  private String category_name_english;
}

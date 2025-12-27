package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategorySuggestionResponse {
    @JsonProperty(value = "category_level")
    private String categoryLevel;

    @JsonProperty(value = "category_id")
    private String categoryId;

    @JsonProperty(value = "category_code")
    private String categoryCode;

    @JsonProperty(value = "category_name")
    private String categoryName;

    @JsonProperty(value = "category_name_english")
    private String categoryNameEnglish;
}

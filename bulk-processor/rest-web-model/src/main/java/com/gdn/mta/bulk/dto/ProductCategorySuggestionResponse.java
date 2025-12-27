package com.gdn.mta.bulk.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCategorySuggestionResponse {
    @JsonProperty(value = "predicted_categories")
    private List<SuggestedCategoriesResponse> predictedCategories = new ArrayList<>();

    private String query;

    @JsonProperty("other_ecommerce_cat")
    private String externalCategory;
}

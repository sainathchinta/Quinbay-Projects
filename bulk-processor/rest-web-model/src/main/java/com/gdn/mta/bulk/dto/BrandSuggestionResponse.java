package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandSuggestionResponse {

    private List<BrandPredictionResponse> predictions = new ArrayList<>();

    @JsonProperty("brand_recommendation")
    private List<BrandDetailsResponse> brandRecommendation = new ArrayList<>();
}

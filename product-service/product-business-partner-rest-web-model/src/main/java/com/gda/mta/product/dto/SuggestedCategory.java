package com.gda.mta.product.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.codehaus.jackson.annotate.JsonIgnoreProperties;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SuggestedCategory implements Serializable {
    private String categoryCode;
    private String categoryId;
    private String categoryLevel;
    private String categoryName;
    private String categoryNameEnglish;
}

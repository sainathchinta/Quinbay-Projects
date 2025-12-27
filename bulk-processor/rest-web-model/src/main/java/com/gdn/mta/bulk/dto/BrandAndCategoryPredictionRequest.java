package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAndCategoryPredictionRequest extends GdnBaseDomainEventModel
    implements Serializable {
    private String storeId;
    private String bulkProcessId;
    private String bulkProcessCode;
    private String parentProduct;
    private String externalCategory;
    private String productName;
    private String productDescription;
}

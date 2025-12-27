package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.BrandRestrictedModelsResponse;
import com.gda.mta.product.dto.response.CategoryModelPredictionResponse;
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gda.mta.product.dto.response.KeywordRestrictionModelsResponse;
import com.gda.mta.product.dto.response.RestrictionModelsResponse;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageQcResponseDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -4061731066172514348L;
  private String productCode;
  private List<ImageQcResponse> images = new ArrayList<>();
  private boolean success = false;
  private String errorMessage;
  private List<RestrictionModelsResponse> restrictionModels = new ArrayList<>();
  private List<BrandRestrictedModelsResponse> brandModels = new ArrayList<>();
  private List<KeywordRestrictionModelsResponse> keywordRestrictionModels = new ArrayList<>();
  private List<CategoryModelPredictionResponse> categoryModels = new ArrayList<>();

  public ImageQcResponseDomainEvent(String productCode, List<ImageQcResponse> images,
    boolean success, String errorMessage) {
    this.productCode = productCode;
    this.images = images;
    this.success = success;
    this.errorMessage = errorMessage;
  }

  public ImageQcResponseDomainEvent(String productCode, List<ImageQcResponse> images,
    boolean success, String errorMessage, List<RestrictionModelsResponse> restrictionModels,
    List<BrandRestrictedModelsResponse> brandModels) {
    this.productCode = productCode;
    this.images = images;
    this.success = success;
    this.errorMessage = errorMessage;
    this.restrictionModels = restrictionModels;
    this.brandModels = brandModels;
  }

  public ImageQcResponseDomainEvent(String productCode, List<ImageQcResponse> images,
    boolean success, String errorMessage, List<RestrictionModelsResponse> restrictionModels,
    List<BrandRestrictedModelsResponse> brandModels,
    List<KeywordRestrictionModelsResponse> keywordRestrictionModels) {
    this.productCode = productCode;
    this.images = images;
    this.success = success;
    this.errorMessage = errorMessage;
    this.restrictionModels = restrictionModels;
    this.brandModels = brandModels;
    this.keywordRestrictionModels = keywordRestrictionModels;
  }
}

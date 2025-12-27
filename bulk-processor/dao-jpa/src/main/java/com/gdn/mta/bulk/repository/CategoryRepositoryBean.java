package com.gdn.mta.bulk.repository;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.MapUtils;
import org.apache.http.client.utils.URIBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.ListHolderRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

@Repository
public class CategoryRepositoryBean implements CategoryRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public CategoryDetailResponse findByStoreIdAndCategoryCodeAndMarkDeleteFalse(String storeId, String categoryCode)
      throws Exception {
    GdnRestSingleResponse<CategoryDetailResponse> response =
        pcbFeign.getCategoryDetailByCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    return response.getValue();
  }
  
  @Override
  public List<AllowedAttributeValueDtoResponse> getPredefinedAndDefiningAllowedAttributeValue(
      String username, String requestId, String storeId, List<AllowedAttributeValueDtoRequest> attrRequest) throws Exception {
    ListHolderRequest<AllowedAttributeValueDtoRequest> request = new ListHolderRequest<>(attrRequest);
    GdnRestListResponse<AllowedAttributeValueDtoResponse> result =
        pcbFeign.getPredefinedAndDefiningAllowedAttributeValue(storeId,
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), requestId, username, request);
    this.checkOutboundResponse(result.isSuccess(), result.getErrorMessage());
    return result.getContent();
  }
  
  @Override
  public boolean validateIsCnCategory(String storeId, String categoryCode)
      throws Exception {
    GdnRestSingleResponse<CategoryResponse> response =
        pcbFeign.validateIsCnCategory(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    return response.isSuccess();
  }

  @Override
  public List<CategoryResponse> filterCategoryHierarchyByCategoryCode(String storeId,
      String categoryCode) throws Exception {
    GdnRestListResponse<CategoryResponse> response =
        pcbFeign.filterCategoryHierarchyByCategoryCode(storeId,
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryCode);
    return response.getContent();
  }

  @Override
  public BrandResponse getBrandDetail(String username, String requestId, String brandName) throws Exception {
    GdnRestSingleResponse<BrandResponse> result =
        pcbFeign.filterByBrandName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), brandName,
            false, false);
    return result.getValue();
  }
  
  private void checkOutboundResponse(boolean isSuccess, String errorMsg) throws Exception {
    if(!isSuccess) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error fetching from PCB " + errorMsg);
    }
  }
}

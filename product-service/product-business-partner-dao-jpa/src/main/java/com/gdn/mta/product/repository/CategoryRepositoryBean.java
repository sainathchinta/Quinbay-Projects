package com.gdn.mta.product.repository;

import java.util.Arrays;
import java.util.List;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;

@Repository
public class CategoryRepositoryBean implements CategoryRepository {

  @Autowired
  private PCBFeign pcbFeign;
  
  @Override
  public Category findOne(String id) throws Exception {
    CategoryDetailResponse categoryDetailResponse =
        pcbFeign.getCategoryDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
                GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), id)
            .getValue();
    Category category = new Category();
    Catalog catalog = new Catalog();
    BeanUtils.copyProperties(categoryDetailResponse, category);
    BeanUtils.copyProperties(categoryDetailResponse.getCatalog(), catalog);
    catalog.setCatalogType(CatalogType
        .valueOf(categoryDetailResponse.getCatalog().getCatalogType()));
    category.setCatalog(catalog);
    return category;
  }

  @Override
  public List<CategoryResponse> findHierarchyByCategoryCode(String categoryCode) throws Exception {
    GdnRestListResponse<CategoryResponse> response =
        pcbFeign.filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    List<CategoryResponse> categories = response.getContent();
    return categories;
  }

  @Override
  public List<CategoryHierarchyResponse> findHierarchyByCategoryCodes(CategoryCodeRequest request)
      throws Exception {
    GdnRestListResponse<CategoryHierarchyResponse> response =
        pcbFeign.filterCategoryHierarchyByCategoryCodes(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategoryCached(String requestId,
      String username, String categoryId) throws Exception {
    
    try{
      GdnRestSingleResponse<SingleObjectResponse> response =
          pcbFeign.getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              requestId, username, categoryId);
      
      if(! response.isSuccess()){
        throw new Exception("Final parent category response is null");
      }
      return response;
    } catch(Exception e){
      throw new Exception("Category client connection problem: " + e.getMessage());
    }
    
  }

  @Override
  public List<String> getAllChildCategoriesByC1CategoryCode(String requestId, String username, String categoryCode)
      throws Exception {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    categoryCodeRequest.setCategoryCodes(Arrays.asList(categoryCode));
    GdnRestSingleResponse<CategoryCodeResponse> childCategories =
        pcbFeign.getAllChildCategoriesFromC1CategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, false, categoryCodeRequest);
    return childCategories.getValue().getCategoryCodes();
  }
}

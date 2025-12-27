package com.gdn.mta.bulk.repository.pcb;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;

@Repository
public class ProductCategoryBaseRepositoryBean implements ProductCategoryBaseRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public List<String> getAllChildCategoriesFromC1CategoryCode(String requestId, String username,
      List<String> categoryCodes) throws Exception {
    GdnRestSingleResponse<CategoryCodeResponse> response =
        pcbFeign.getAllChildCategoriesFromC1CategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            false, new CategoryCodeRequest(categoryCodes));
    return Optional.ofNullable(response).map(GdnRestSingleResponse::getValue)
        .map(CategoryCodeResponse::getCategoryCodes).orElse(Collections.emptyList());
  }

}

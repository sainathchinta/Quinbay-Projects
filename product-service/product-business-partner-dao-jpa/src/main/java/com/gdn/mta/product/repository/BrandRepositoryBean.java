package com.gdn.mta.product.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

@Repository
public class BrandRepositoryBean implements BrandRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public BrandResponse findBrandByBrandName(String brandName) throws Exception {
    GdnRestSingleResponse<BrandResponse> response =
        pcbFeign.filterByBrandName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), brandName,
            false, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    BrandResponse brandResponse = response.getValue();
    return brandResponse;
  }
}

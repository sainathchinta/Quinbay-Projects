package com.gdn.mta.product.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;

@Repository
public class BrandWipRepositoryBean implements BrandWipRepository {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public BrandWipResponse findBrandWipByBrandNameAndBusinessPartnerCode(String brandName, String businessPartnerCode) throws Exception {
    GdnRestSingleResponse<BrandWipResponse> response =
        pcbFeign.findBrandWipByBrandNameAndBusinessPartnerCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), brandName,
            businessPartnerCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    BrandWipResponse brandWipResponse = response.getValue();
    return brandWipResponse;
  }
}

package com.gdn.partners.pbp.distributiontask;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.partners.pbp.dao.IPredefinedAllowedAttributeValueDao;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

@Service
@Qualifier("predefinedAttributeAllowedValueService")
public class PredefinedAttributeAllowedValueServiceBean implements PredefinedAttributeAllowedValueService {

  @Autowired
  private IPredefinedAllowedAttributeValueDao dao;

  @Override
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(
      String requestId, GdnRestListRequest listRequest, String attributeId, String value)
      throws Exception {
    return dao.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(requestId, listRequest,
        attributeId, value);
  }

  @Override
  public PredefinedAllowedAttributeValueResponse findByStoreIdAndMatchAttributeCodeAndValue(
      String requestId, String attributeCode, String value) throws Exception {
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> response =
        this.dao.findByStoreIdAndMatchAttributeCodeAndValue(requestId, attributeCode, value);
    if (!response.isSuccess()) {
      throw new IllegalArgumentException("error from Master Data Product : "
          + response.getErrorMessage());
    }
    return response.getValue();
  }

}

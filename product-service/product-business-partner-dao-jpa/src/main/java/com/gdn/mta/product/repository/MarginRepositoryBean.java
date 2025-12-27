package com.gdn.mta.product.repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.pbp.outbound.margin.feign.MarginFeign;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

@Repository
@Slf4j
public class MarginRepositoryBean implements MarginRepository {

  @Autowired
  private MarginFeign marginFeign;

  @Override
  public MarginCategoryResponse getMarginForStoreIdAndCategoryCode(String storeId, String categoryCode)
      throws Exception {
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(
        "yyyy-MM-dd HH:mm:ss");
    GdnRestSingleResponse<MarginCategoryResponse> response =
        this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(categoryCode,
            simpleDateFormat.format(new Date()),
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername());
      validateResponse(response);
      return response.getValue();
  }

  private void validateResponse(GdnRestSingleResponse<MarginCategoryResponse> response)
      throws Exception {
    if(Objects.isNull(response)){
      log.error("margin category Response is Null");
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND ,"MarginCategoryResponse is null");
    }
    if(!response.isSuccess()){
      log.error("margin Category Response failed!!");
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND , "Request failed for margin");
    }
  }
}

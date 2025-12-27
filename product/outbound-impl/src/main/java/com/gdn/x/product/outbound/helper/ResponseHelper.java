package com.gdn.x.product.outbound.helper;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.solr.ProductSolr;

import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.data.domain.Page;

/**
 * Created by w.william on 3/15/2018.
 */
public class ResponseHelper {

  public static <T extends BaseResponse> boolean isValid(GdnRestListResponse<T> response){

    if (response.isSuccess() && response.getContent() != null) {
      return true;
    }
    return false;
  }

  public static <T extends BaseResponse> boolean isValid(GdnRestSingleResponse<T> response){

    if (response.isSuccess() && response.getValue() != null) {
      return true;
    }
    return false;
  }

  public static boolean validateSolrResponse(Page<ProductSolr> productSolrs) {

    if (Objects.nonNull(productSolrs) && CollectionUtils.isNotEmpty(productSolrs.getContent())) {
      return true;
    }
    return false;
  }
}

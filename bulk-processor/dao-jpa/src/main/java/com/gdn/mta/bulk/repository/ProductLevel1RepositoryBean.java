package com.gdn.mta.bulk.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;

//TODO This class is to be removed as it is not used anymore, for now removed the client sdk usage

@Service
public class ProductLevel1RepositoryBean implements ProductLevel1Repository {

  @Override
  public GeneratedProductImagesPathResponse replaceProductImages(
      String requestId, String username, String businessPartnerCode,
      ReplaceProductImagesRequest productImageRequest) throws Exception {
    try{
      GdnRestSingleResponse<GeneratedProductImagesPathResponse> response =
          new GdnRestSingleResponse<>();
      
      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
      }
      return response.getValue();
    } catch(Exception e){
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, "PBP client error: " + e.getMessage());
    }
  }


  @Override
  public GdnRestListResponse<ReviewProductResponse> getReviewProducts(String username,
      String requestId, SummaryFilterRequest summaryFilterRequest,
      Pageable pageable) throws Exception {
    try {
      GdnRestListResponse<ReviewProductResponse> response = new GdnRestListResponse<>();

      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
      }
      return response;
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "PBP client error: " + e.getMessage());
    }
  }

}

package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by virajjasani on 25/09/16.
 */
public interface ProductBusinessPartnerService {

  /**
   * Service method for delete product collection in PBP
   *
   * @param requestId
   * @param userName
   * @param rejectProductDTO
   * @throws Exception
   */
  void deleteProductCollection(String requestId, String userName, RejectProductDTO rejectProductDTO)
      throws Exception;

  /**
   * Service method for delete product collection in PBP
   *
   * @param requestId request id , must not blank
   * @param userName user name, must not blank
   * @param needEmailNotification false , if user doesn't want to notify merchant
   * @param rejectProductDTO request body, must not null
   * @throws Exception
   */
  void deleteProductCollection(String requestId, String userName, boolean needEmailNotification,
      RejectProductDTO rejectProductDTO) throws Exception;

  /**
   * Republish from PBP to PDT
   * 
   * @param requestId
   * @param userName
   * @param productCode
   * @throws Exception
   */
  void republishToPDT(String requestId, String userName, String productCode) throws Exception;

  /**
   * get all product details for given list of product codes
   *
   * @param requestId
   * @param username
   * @param productCodes
   * @return
   * @throws Exception
   */
  List<ProductDetailResponse> getProductDetailsByProductCodes(String requestId,
      String username, List<String> productCodes) throws Exception;
  
  /**
   * <p>Get Product workflow states</p>
   * 
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductWorkflowStatusResponse getWorkflowStatus(String productCode) throws Exception;
}

package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface ProductRepository {
  
  ProductDetailResponse findProductDetailByProductCodeAndMarkForDeleteFalse(String productCode) throws Exception;

  void update(ProductRequest product) throws Exception;
  
  String generateBarcode(String requestId) throws Exception;
  
  String create(CreateProductRequest request) throws Exception;
  
  /**
   * create product via mta-api, with additional logic to validate the product's category
   * @param request
   * @return
   * @throws Exception
   */
  String createViaApi(CreateProductRequest request) throws Exception;

  /**
   * Update bulk master data
   * @param request
   * @return
   * @throws Exception
   */
  BulkMasterProductUpdateResponse updateMasterProducts(BulkMasterProductUpdateRequest request)
      throws Exception;

  /**
   * Generate productCode by requestId and username on ProductBusinessPartner
   * 
   * @param requestId
   * @param username
   * @return
   * @throws Exception 
   */
  GdnRestSimpleResponse<String> generateProductCode(String requestId, String username)
      throws Exception;
  
  /**
   * Create product by requestId, username and ProductCreationRequest on ProductBusinessPartner
   *
   * @param requestId
   * @param username
   * @param request
   * @return
   * @throws Exception
   */
  GdnBaseRestResponse createProduct(String requestId, String username, 
      ProductCreationRequest request) throws Exception;

  /**
   * @param storeId
   * @param requestId
   * @return
   * @throws Exception
   */
  List<String> fetchNRBusinessPartnerCodes(String storeId, String requestId);

  /**
   * check for Eligibility For Need Revision Product Deletion
   * @param storeId non null
   * @return NeedRevisionEligibilityResponse
   */
  List<NeedRevisionEligibilityResponse> getEligibilityForNeedRevisionDeletion(String storeId,
    List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequest)
    throws ApplicationException;

  /**
   * PBP API to delete Product from PBP and PCB
   * @param storeId non null
   * @param deleteProductRequest product code for deletion
   * @param needEmailNotification boolean
   * @return NeedRevisionEligibilityResponse
   */
  boolean deleteProductCollection(String storeId, DeleteProductRequest deleteProductRequest,
    boolean needEmailNotification);
}

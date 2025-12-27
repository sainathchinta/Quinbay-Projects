package com.gdn.mta.bulk.service;

import java.util.List;

import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameResponseList;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface PBPOutboundService {

  /**
   * API to get list of active brands for a category
   * @param categoryId
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> getActiveBrandsByCategoryId(String categoryId) throws Exception;

  /**
   * find Product Details From L5
   *
   * @param businessPartnerCode
   * @param request
   * @param pageable
   * @param needCorrection
   * @return
   * @throws Exception
   */
  Page<ItemPickupPointListingL3Response> findProductDetailsFromL5(String businessPartnerCode,
      ProductLevel3SummaryRequest request, Pageable pageable, boolean needCorrection) throws Exception;

  /**
   * update using quick edit api L5
   *
   * @param productLevel3QuickEditV2Request
   * @param productSku
   * @return
   */
  boolean listingUpdate(ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request, String productSku) throws Exception;

  /**
   * Fetch paginated response of products which are not yet active for seller
   *
   * @param requestId
   * @param username
   * @param businessPartnerCode
   * @param pageable
   * @return
   */
  Page<InProgressProductResponse> fetchInProgressProductsByMerchantCode(String requestId, String username,
    String businessPartnerCode, Pageable pageable);

  /**
   * @param productSku
   * @param productLevel3ViewConfigStockRequest
   */
  void updateProductItemViewConfig(String productSku,
      ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest) throws Exception;

  /**
   * Fetch ProductLevel3Summary response from L5 by productSku List
   *
   * @param page
   * @param size
   * @param businessPartnerCode
   * @param onlineOrCnc
   * @param productSkuList
   * @return
   */
  Page<ProductLevel3SummaryResponse> fetchProductLevel3SummaryByProductSkuList(int page, int size,
    String businessPartnerCode, boolean onlineOrCnc, List<String> productSkuList)
    throws ApplicationException;

  /**
   * Creating new product with L5 changes
   * @param requestId
   * @param createdBy
   * @param productCreationRequest
   */
  void createNewProduct(String requestId, String createdBy, ProductCreationRequest productCreationRequest) throws Exception;

  /**
   * get L5 listing
   *
   * @param page                            int
   * @param size                            int
   * @param itemPickupPointListingL3Request must not be null
   * @return ItemPickupPointListingL3Response page
   * @throws Exception
   */
  Page<ItemPickupPointListingL3Response> getItemPickupPointListingL3Response(int page, int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception;

  /**
   * update L5 summary details
   *
   * @param createdBy
   * @param productVariantUpdateRequest
   * @param clientHost
   * @return
   * @throws Exception
   */
  ItemsPriceStockImagesUpdateResponse updateSummaryL5(String createdBy,
      ProductVariantUpdateRequest productVariantUpdateRequest, String clientHost) throws Exception;

  /**
   * delete terminated seller products
   * @param productSku
   * @throws Exception
   */
  void deleteTerminatedSellerProducts(String productSku);

  /**
   * delete L5's by pickupPoint code and business partner code
   *
   * @param itemSkuPickupPointRequest
   * @return
   */
  List<DeleteInProgressL5Response> deleteL5ByPickupPointCode(ItemSkuPickupPointRequest itemSkuPickupPointRequest);

  Page<InProgressProductsByPickupPointCodeResponse> getInProgressProductsByPickupPointCode(String businessPartnerCode,
      String pickupPointCode, int page, int size) throws Exception;

  /**
   *
   * @param itemSkuPickupPointRequest
   * @return
   * @throws Exception
   */
  FbbCreatePickupPointResponse createDefaultL5Fbb(FbbCreatePickupPointRequest itemSkuPickupPointRequest)
    throws Exception;

  /**
   *  calling pbp to update product brand
   * @param storeId
   * @param productBrandUpdateRequest
   * @throws Exception
   */
  void updateBrandOfProduct(String storeId, ProductBrandUpdateRequest productBrandUpdateRequest) throws Exception;

  /**
   * calling pbp to update product brand name
   *
   * @param internalBrandUpdateEventModel
   * @param bulkInternalProcessData
   * @return GdnBaseRestResponse
   * @throws Exception
   */
  GdnBaseRestResponse updateProductBrandName(
      InternalBrandUpdateEventModel internalBrandUpdateEventModel,
      BulkInternalProcessData bulkInternalProcessData) throws Exception;

  /**
   * GET products of a bp from PBP
   * @param storeId
   * @param requestId
   * @param businessPartnerCode
   */
  ProductCodeAndNameResponseList getProductDetailsOfBusinessPartnerFromPBP(String storeId, String requestId, String businessPartnerCode);

  /**
   * fetch product master data edit info
   *
   * @param productSku
   * @param productMasterDataEditRequest
   * @param username
   */
  void updatedProductMaterData(String productSku,
      ProductMasterDataEditRequest productMasterDataEditRequest, String username);

  /**
   * fetch products by brand name from pbp
   *
   * @param page
   * @param destinationBrandName
   * @param storeId
   * @return
   */
  GdnRestListResponse<ProductAndBrandResponse> getProductAndBrandResponseGdnRestListResponse(
      int page, String destinationBrandName, String storeId);

}

package com.gdn.x.mta.distributiontask.service.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.partners.pdt.dto.configuration.distribution.IPRActionResponseDto;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.AddingIprProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.IPRHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprSuspensionInProgressResponse;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;

public interface IprService {

  /**
   * @param storeId             String
   * @param businessPartnerCode String
   * @param page                int
   * @param size                int
   * @param sortOrder           String
   * @return Page<IprSuspensionInProgressResponse>
   * @throws Exception Exception
   */
  Page<IprSuspensionInProgressResponse> findSuspensionInProgressProducts(String storeId,
    String businessPartnerCode, int page, int size, String sortOrder) throws Exception;

  /**
   * get ipr product list response
   *
   * @param storeId               String
   * @param iprProductListRequest IPRProductListRequest
   * @param pageable              Pageable
   * @return Page<IprProductListResponse>
   * @throws Exception Exception
   */
  Page<IprProductListResponse> getIprProductListResponse(String storeId,
    IPRProductListRequest iprProductListRequest, Pageable pageable) throws Exception;

  /**
   * updating product on listening to product change event from x-product
   *
   * @param productChangeEventModel ProductChange
   */
  Pair<ProductIPR, IPRHistoryEventModel> updateProductOnStateChange(ProductChange productChangeEventModel)
      throws Exception;

  /**
   * find by product sku
   *
   * @param productSku String
   * @return ProductIPR
   */
  ProductIPR findByProductSku(String productSku);

  /**
   * find by product sku and mfd flag
   *
   * @param productSku    String
   * @param markForDelete boolean
   * @return ProductIPR
   */
  ProductIPR findByProductSkuAndMarkForDelete(String productSku, boolean markForDelete);

  /**
   * to add product to IPR
   *
   * @param productSku String
   * @param storeId String
   * @param source String
   * @param assignee String
   * @param brandReport BrandReport
   * @return AddingIprProductDTO
   */
  AddingIprProductDTO addProductToIPR(String productSku, String storeId, String source,
      String assignee, BrandReport brandReport) throws Exception;

  /**
   * submit evidence for a product
   *
   * @param submitEvidenceRequest SubmitEvidenceRequest
   * @return ProductIPR
   */
  Pair<ProductIPR, IPRHistoryEventModel> submitEvidenceForProduct(SubmitEvidenceRequest submitEvidenceRequest)
      throws JsonProcessingException;

  /**
   * Fetching IPR Product Details
   *
   * @param productSku productSku
   * @return IprProductDetailsResponse
   */
  IprProductDetailsResponse fetchIprProductDetails(String productSku);

  /**
   * Update assignee for the product
   *
   * @param iprUpdateAssigneeRequest
   */
  Pair<List<ProductIPR>, List<IPRHistoryEventModel>> updateAssignee(
      IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws Exception;

  /**
   * to perform ipr actions for a product sku
   *
   * @param iprActionRequest IprActionRequest
   * @param storeId String
   * @return IPRActionResponseDto
   */
  IPRActionResponseDto performIprActionForProduct(IprActionRequest iprActionRequest, String storeId)
    throws Exception;

  /**
   * get primary filter counts for IPR portal
   *
   * @param storeId String
   * @return Map<String, Object>
   * @throws Exception Exception
   */
  Map<String, Object> getPrimaryFilterCounts(String storeId) throws Exception;

  /**
   * Fetch and suspend evidence requested product passed threshold
   *
   * @param storeId
   * @param daysThreshold
   * @param pageable
   */
  void fetchAndSuspendEvidenceRequestedProduct(String storeId, int daysThreshold,
    Pageable pageable);

  /**
   * Fetch the history of IPR product by productSku
   *
   * @param storeId
   * @param productSku
   * @param page
   * @return
   */
  Page<IPRHistoryResponse> fetchIprHistoryByProductSku(String storeId, String productSku,
      Pageable page);

  /**
   * update ipr product history
   *
   * @param storeId              String
   * @param iprHistoryEventModel IPRHistoryEventModel
   */
  void updateIprHistoryForProduct(String storeId, IPRHistoryEventModel iprHistoryEventModel);

  /**
   *
   * @param productSku
   * @param storeId
   * @param source
   * @return
   */
  AddingIprProductDTO addDSModelProductToIPR(String productSku, String storeId, String source);
}

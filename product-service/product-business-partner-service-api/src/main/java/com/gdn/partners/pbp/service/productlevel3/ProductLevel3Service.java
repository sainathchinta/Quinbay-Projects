package com.gdn.partners.pbp.service.productlevel3;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductLevel3ImageBundle;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface ProductLevel3Service {


  /**
   * Update Sync stock for all product by business partner
   * @param businessPartnerCode
   * @param syncStock
   * @throws Exception
   */
  void updateSyncStockByBusinessPartnerCode(String businessPartnerCode, boolean syncStock)
      throws Exception;

  /**
   * update product Item Sync Stock
   * @param businessPartnerCode
   * @param gdnSku
   * @param syncStock
   * @throws Exception
   */
  void updateSyncStockByBusinessPartnerCodeAndGdnSku(String businessPartnerCode, String gdnSku,
      boolean syncStock) throws Exception;

  Page<ProductLevel3Summary> findSummaryByFilter(ProductLevel3SummaryFilter filterRequest,
      Pageable pageRequest, SortOrder sort) throws Exception;

  Page<ProductLevel3SummaryMinified> findSummaryMinifiedByFilter(
      ProductLevel3SummaryFilter filterRequest,
      Pageable pageRequest, SortOrder sort) throws Exception;

  Page<ProductLevel3ImageBundle> findImageBundleByFilter(ItemSummaryRequest filter, Pageable pageRequest,
      SortOrder sort) throws Exception;

  /**
   * Update all items resign business partner
   * @param storeId
   * @param businessPartnerCode
   */
  void updateResignBusinessPartnerItems(String storeId, String businessPartnerCode) throws Exception;

  Page<ProductLevel3Item> findItemBySearch(ProductLevel3ItemSearch search, Pageable pageable,
      SortOrder sort) throws Exception;

  Boolean checkAvailableStock(String businessPartnerCode) throws Exception;

  /**
   * Filter products from source {@link ProductLevel3SummaryFilter#getBusinessPartnerCode()} account which is eligible
   * for copying to partner account business partner code
   *
   * @param filterRequest       products filter request
   * @param businessPartnerCode partner code to which products will be copied
   * @param pageRequest         paging request
   *
   * @return product summary page
   *
   * @throws Exception
   */
  Page<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopied(ProductLevel3SummaryFilter filterRequest,
    String businessPartnerCode, Pageable pageRequest) throws Exception;

  /**
   * take down need for correction product
   *
   * @param productCode
   * @param itemCodeIdMap
   * @param productDetailResponse
   */
  void takeDownNeedForCorrectionProduct(String productCode, Map<String, String> itemCodeIdMap,
      ProductDetailResponse productDetailResponse)
      throws Exception;

  /**
   * update vat external histiry in db and solr
   * @param vatUpdateHistoryDomainEventModel
   */
  void addVatUpdateExternalHistory(VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel)
      throws JsonProcessingException;

  /**
   *
   * @param auditTrailRequests
   */
  void setProductNameInHistoryIfEmpty(List<AuditTrailDto> auditTrailRequests) throws ApplicationException;
}

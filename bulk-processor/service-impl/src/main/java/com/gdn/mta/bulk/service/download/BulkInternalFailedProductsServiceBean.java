package com.gdn.mta.bulk.service.download;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkInternalProcessDataPayload;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.InternalBulkProcessFailedData;
import com.gdn.mta.bulk.models.SalesCategoryUpdateRequest;
import com.gdn.mta.bulk.models.StoreCopyFailedProducts;
import com.gdn.mta.bulk.models.UpdateSalesCategoryFailedProduct;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.service.InternalProcessService;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service(value = "bulkInternalFailedProductsServiceBean")
public class BulkInternalFailedProductsServiceBean implements BulkProcessDataService{

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    InternalProcessFailedProductsDownloadRequest internalProcessFailedProductsDownloadRequest =
        (InternalProcessFailedProductsDownloadRequest) request;
    log.info("Fetching failed products data for internal process internal-process-request-code {}",
        internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode());
    List<BulkInternalProcessData> bulkInternalProcessDataList = internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(Constant.STORE_ID,
            internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode(), ProcessStatus.FAILED.name());
    InternalProcessFailedProductResponse internalProcessFailedProductResponse =
        new InternalProcessFailedProductResponse();
    internalProcessFailedProductResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    if (BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS.equals(request.getBulkProcessEntity())) {
      internalProcessFailedProductResponse
          .setInternalBulkProcessFailedData(generateStoreCopyFailedProduct(bulkInternalProcessDataList));
    } else if (BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS.equals(request.getBulkProcessEntity())) {
      internalProcessFailedProductResponse
          .setInternalBulkProcessFailedData(generateUpdateSalesCategoryFailedProduct(bulkInternalProcessDataList));
    }
    return internalProcessFailedProductResponse;
  }

  private List<InternalBulkProcessFailedData> generateStoreCopyFailedProduct(
      List<BulkInternalProcessData> bulkInternalProcessDataList) throws IOException {
    List<InternalBulkProcessFailedData> storeCopyFailedProductsList = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
      BulkInternalProcessDataPayload bulkInternalProcessDataPayload =
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkInternalProcessDataPayload.class);
      storeCopyFailedProductsList.add(
          StoreCopyFailedProducts.builder().productCode(bulkInternalProcessDataPayload.getProductCode())
              .productSku(bulkInternalProcessDataPayload.getProductSku())
              .productName(bulkInternalProcessDataPayload.getProductName())
              .itemCode(bulkInternalProcessDataPayload.getItemCode())
              .itemSku(bulkInternalProcessDataPayload.getItemSku())
              .copyProductName(bulkInternalProcessDataPayload.getCopyProductName())
              .listPrice(bulkInternalProcessDataPayload.getListPrice())
              .offerPrice(bulkInternalProcessDataPayload.getOfferPrice())
              .minimumStock(bulkInternalProcessDataPayload.getMinimumStock())
              .stock(bulkInternalProcessDataPayload.getStock())
              .shippingType(bulkInternalProcessDataPayload.getShippingType())
              .errorMessage(bulkInternalProcessData.getErrorMessage()).build());
    }
    return storeCopyFailedProductsList;
  }

  private List<InternalBulkProcessFailedData> generateUpdateSalesCategoryFailedProduct(
      List<BulkInternalProcessData> bulkInternalProcessDataList) throws IOException {
    List<InternalBulkProcessFailedData> updateSalesCategoryFailedProducts = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
      SalesCategoryUpdateRequest salesCategoryUpdateRequest =
          objectMapper.readValue(bulkInternalProcessData.getData(), SalesCategoryUpdateRequest.class);
      updateSalesCategoryFailedProducts.add(
          UpdateSalesCategoryFailedProduct.builder().productSku(bulkInternalProcessData.getParentCode())
              .operationType(salesCategoryUpdateRequest.getOperationType())
              .cnCategoryCode(salesCategoryUpdateRequest.getCnCategoryCode())
              .errorMessage(bulkInternalProcessData.getErrorMessage()).build());
    }
    return updateSalesCategoryFailedProducts;
  }
}

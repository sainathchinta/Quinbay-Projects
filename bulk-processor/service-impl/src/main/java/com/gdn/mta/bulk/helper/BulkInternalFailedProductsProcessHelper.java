package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.InternalBulkProcessFailedData;
import com.gdn.mta.bulk.models.StoreCopyFailedProducts;
import com.gdn.mta.bulk.models.UpdateSalesCategoryFailedProduct;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.SalesCategoryUpdateConstants;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

@Component(value = "bulkInternalFailedProductsProcessHelper")
public class BulkInternalFailedProductsProcessHelper extends BulkProcessHelper {

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  private static final List<String> STORE_COPY_HEADER_LIST =
      ImmutableList.<String>builder().add(StoreCopyConstants.BLIBLI_PRODUCT_SKU)
          .add(StoreCopyConstants.PARENT_PRODUCT_NAME).add(StoreCopyConstants.BLIBLI_SKU)
          .add(StoreCopyConstants.NAMA_PRODUK).add(StoreCopyConstants.SKU_CODE).add(StoreCopyConstants.PRODUCT_CODE)
          .add(StoreCopyConstants.COPIED_PRODUCT_NAME).add(StoreCopyConstants.SELLER_SKU)
          .add(StoreCopyConstants.HATGA).add(StoreCopyConstants.HARGA_PENJUALAN)
          .add(StoreCopyConstants.STOK).add(StoreCopyConstants.TOKO_GUDANG).add(StoreCopyConstants.MINIMUM_STOCK)
          .add(StoreCopyConstants.SKU_STATUS).add(StoreCopyConstants.SHIPPING_TYPE)
          .add(StoreCopyConstants.ERROR_HEADER).build();

  private static final List<String> UPDATE_SALES_CATEGORY_HEADER_LIST =
      ImmutableList.<String>builder().add(SalesCategoryUpdateConstants.PRODUCT_SKU)
          .add(SalesCategoryUpdateConstants.OPERATION_TYPE).add(SalesCategoryUpdateConstants.CATEGORY_CODE).build();

  private static final Map<BulkProcessEntity, List<String>> INTERNAL_PROCESS_HEADER_MAP =
      ImmutableMap.<BulkProcessEntity, List<String>>builder()
          .put(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS, STORE_COPY_HEADER_LIST)
          .put(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS, UPDATE_SALES_CATEGORY_HEADER_LIST).build();

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    return null;
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    return INTERNAL_PROCESS_HEADER_MAP.get(bulkDataResponse.getBulkProcessEntity());
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return workbook;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    InternalProcessFailedProductResponse internalProcessFailedProductResponse =
        (InternalProcessFailedProductResponse) response;
    List<List<String>> rowData = new ArrayList<>();
    if (BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS
        .equals(internalProcessFailedProductResponse.getBulkProcessEntity())) {
      bulkDownloadServiceBeanUtil.generateStoreCopyRows(internalProcessFailedProductResponse, rowData);
    } else if (BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS
        .equals(internalProcessFailedProductResponse.getBulkProcessEntity())) {
      bulkDownloadServiceBeanUtil.generateUpdateSalesCategoryRows(internalProcessFailedProductResponse, rowData);
    }
    return rowData;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    InternalProcessFailedProductsDownloadRequest internalProcessFailedProductsDownloadRequest =
        (InternalProcessFailedProductsDownloadRequest) request;
    return ProcessorUtils.ENTITY_DIR_MAP.get(internalProcessFailedProductsDownloadRequest.getBulkProcessEntity())
        + internalProcessFailedProductsDownloadRequest.getInternalProcessRequestCode();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    return null;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    return 0;
  }
}

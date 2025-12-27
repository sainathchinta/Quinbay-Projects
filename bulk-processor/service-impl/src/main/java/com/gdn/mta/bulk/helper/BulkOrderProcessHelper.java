package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkOrderResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.neo.order.client.sdk.model.OrderType;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by keshashah on 25/10/16.
 */
@Slf4j
@Component(value = "bulkOrderProcessHelper")
public class BulkOrderProcessHelper extends BulkProcessHelper {
  private static final String ORDER_NO = "No. Order";
  private static final String ORDER_ITEM_NO = "No. Order Item";
  private static final String PACKAGE_ID = "No. Paket";
  private static final String AWB_NO = "No. Awb";
  private static final String TANGGAL_ORDER = "Tanggal Order";
  private static final String NAME_PEMESAN = "Nama Pemesan";
  private static final String SKU_CODE = "Kode SKU";
  private static final String BLIBLI_SKU = "Blibli SKU";
  private static final String MERCHANT_SKU = "Merchant SKU";
  private static final String PRODUCT_NAME = "Nama Produk";
  private static final String TOTAL_BARANG = "Total Barang";
  private static final String PRODUCT_PRICE = "Harga Produk";
  private static final String LOGISTIC_SERVICE = "Servis Logistik";
  private static final String MERCHANT_CODE = "Kode Merchant";
  private static final String STORE_NAME = "Nama Store";
  private static final String ORDER_STATUS = "Order Status";
  private static final String PICKUP_POINT = "Alamat pengambilan";
  private static final String KETERANGAN = "Keterangan";
  private static final String ORDER_HANDED_OVER = "Pesanan Diserahkan";
  private static final String ORDER_TYPE = "Tipe pesanan";
  private static final String NOTES_TO_SELLER = "Catatan produk";
  private static final String SHIPPING_STREET_ADDRESS = "Alamat";
  private static final String SHIPPING_CITY = "Kota";
  private static final String SHIPPING_PROVINCE = "Provinsi";
  private static final String ORDER_ITEM_PRICE = "Harga item pesanan";
  private static final String TOTAL_ORDER_ITEM_PRICE = "Total harga item pesanan";
  private static final String TOTAL_ORDER_PRICE = "Total";
  private static final String SELLER_DISCOUNT = "Diskon";
  private static final String DISCOUNT_NAME = "Nama diskon";
  private static final String DISCOUNT_CODE = "Kode diskon";
  private static final String SELLER_VOUCHER = "Voucher seller";
  private static final String MERCHANT_VOUCHER_NAME = "Nama voucher seller";
  private static final String MERCHANT_VOUCHER_CODE = "Kode voucher seller";
  private static final String SHIPPING_VOUCHER_NAME = "Nama voucher gratis ongkir";
  private static final String SHIPPING_VOUCHER_CODE = "Kode voucher gratis ongkir";
  private static final String STATUS_D_UPDATED_TIMESTAMP = "Tanggal pengiriman";
  private static final String CANCELLED_BY = "Dibatalkan oleh";
  private static final String CANCELLATION_REASON = "Alasan Pembatalan";
  private static final String IS_SUPERMARKETVERIFIED = "Supermarket Terverifikasi";
  private static final String PAYMENT_DISPLAY_NAME = "Metode pembayaran";
  private static final String RMA_NUMBER = "No. RMA";
  private static final String RETURN_STATUS = "Status pengembalian";
  private static final String RETURN_RESOLUTION = "Resolusi pengembalian";
  private static final String RETURN_REASON = "Alasan pengembalian";
  private static final String COMMISSION = "Komisi";
  private static final String SALESMAN_EMAIL = "E-mail Sales pendamping";
  private static final String SALESMAN_NAME = "Nama Sales pendamping";

  // Field name constants
  private static final String FIELD_ORDER_ID = "orderId";
  private static final String FIELD_ORDER_ITEM_ID = "orderItemId";
  private static final String FIELD_AWB_NUMBER = "awbNumber";
  private static final String FIELD_STATUS_FP_UPDATED_TIMESTAMP = "statusFPUpdatedTimestamp";
  private static final String FIELD_CUSTOMER_FULL_NAME = "customerFullName";
  private static final String FIELD_SKU_CODE = "skuCode";
  private static final String FIELD_ITEM_SKU = "itemSku";
  private static final String FIELD_MERCHANT_SKU = "merchantSku";
  private static final String FIELD_ITEM_NAME = "itemName";
  private static final String FIELD_QUANTITY = "quantity";
  private static final String FIELD_PRICE = "price";
  private static final String FIELD_LOGISTICS_PRODUCT_NAME = "logisticsProductName";
  private static final String FIELD_MERCHANT_CODE = "merchantCode";
  private static final String FIELD_STORE_NAME = "storeName";
  private static final String FIELD_ORDER_ITEM_STATUS = "orderItemStatus";
  private static final String FIELD_PICKUP_POINT_NAME = "pickupPointName";
  private static final String FIELD_PACKAGE_ID = "packageId";
  private static final String FIELD_SHIPPING_STREET_ADDRESS = "shippingStreetAddress";
  private static final String FIELD_SHIPPING_CITY = "shippingCity";
  private static final String FIELD_SHIPPING_PROVINCE = "shippingProvince";
  private static final String FIELD_TOTAL_ORDER_ITEM_PRICE = "totalOrderItemPrice";
  private static final String FIELD_TOTAL_ORDER_PRICE = "totalOrderPrice";
  private static final String FIELD_SELLER_DISCOUNT = "sellerDiscount";
  private static final String FIELD_DISCOUNT_NAME = "discountName";
  private static final String FIELD_DISCOUNT_CODE = "discountCode";
  private static final String FIELD_SELLER_VOUCHER = "sellerVoucher";
  private static final String FIELD_SELLER_COMMISSION = "sellerCommission";
  private static final String FIELD_MERCHANT_VOUCHER_NAME = "merchantVoucherName";
  private static final String FIELD_MERCHANT_VOUCHER_CODE = "merchantVoucherCode";
  private static final String FIELD_SHIPPING_VOUCHER_NAME = "shippingVoucherName";
  private static final String FIELD_SHIPPING_VOUCHER_CODE = "shippingVoucherCode";
  private static final String FIELD_ITEM_NOTES = "itemNotes";
  private static final String FIELD_STATUS_D_UPDATED_TIMESTAMP = "statusDUpdatedTimestamp";
  private static final String FIELD_CANCELLED_BY = "cancelledBy";
  private static final String FIELD_CANCELLATION_REASON = "cancellationReason";
  private static final String FIELD_RMA_NUMBER = "rmaNumber";
  private static final String FIELD_RETURN_STATUS = "returnStatus";
  private static final String FIELD_RETURN_REASON = "returnReason";
  private static final String FIELD_RETURN_ACTUAL_RESOLUTION = "returnActualResolution";
  private static final String FIELD_ORDER_TYPE = "orderType";
  private static final String FIELD_SALES_PERSON_NAME = "salesPersonName";
  private static final String FIELD_HANDED_OVER = "handedOver";
  private static final String FIELD_SUPERMARKET_VERIFIED = "supermarketVerified";
  private static final String FIELD_PAYMENT_METHOD = "paymentMethod";
  private static final String FIELD_SALESMAN_NAME = "salesmanName";
  private static final String FIELD_SALESMAN_EMAIL = "salesmanEmail";

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${handover.feature.switch}")
  private boolean handoverFeatureSwitch;

  private static final ImmutableMap<String, String> ONLINE_SOLR_HEADER_TO_FIELD_MAP =
      new ImmutableMap.Builder<String, String>()
      .put(ORDER_NO, FIELD_ORDER_ID)
      .put(ORDER_ITEM_NO, FIELD_ORDER_ITEM_ID)
      .put(AWB_NO, FIELD_AWB_NUMBER)
      .put(TANGGAL_ORDER, FIELD_STATUS_FP_UPDATED_TIMESTAMP)
      .put(NAME_PEMESAN, FIELD_CUSTOMER_FULL_NAME)
      .put(SKU_CODE, FIELD_SKU_CODE)
      .put(BLIBLI_SKU, FIELD_ITEM_SKU)
      .put(MERCHANT_SKU, FIELD_MERCHANT_SKU)
      .put(PRODUCT_NAME, FIELD_ITEM_NAME)
      .put(TOTAL_BARANG, FIELD_QUANTITY)
      .put(PRODUCT_PRICE, FIELD_PRICE)
      .put(LOGISTIC_SERVICE, FIELD_LOGISTICS_PRODUCT_NAME)
      .put(MERCHANT_CODE, FIELD_MERCHANT_CODE)
      .put(STORE_NAME, FIELD_STORE_NAME)
      .put(ORDER_STATUS, FIELD_ORDER_ITEM_STATUS)
      .put(PICKUP_POINT, FIELD_PICKUP_POINT_NAME)
      .build();

  private static final List<String> SOLR_HEADER_LIST =
      new ArrayList<>(ONLINE_SOLR_HEADER_TO_FIELD_MAP.keySet());

  private static final ImmutableMap<String, String> ONLINE_HEADER_TO_FIELD_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(ORDER_NO, FIELD_ORDER_ID)
          .put(ORDER_ITEM_NO, FIELD_ORDER_ITEM_ID)
          .put(PACKAGE_ID, FIELD_PACKAGE_ID)
          .put(AWB_NO, FIELD_AWB_NUMBER)
          .put(TANGGAL_ORDER, FIELD_STATUS_FP_UPDATED_TIMESTAMP)
          .put(NAME_PEMESAN, FIELD_CUSTOMER_FULL_NAME)
          .put(SKU_CODE, FIELD_SKU_CODE)
          .put(BLIBLI_SKU, FIELD_ITEM_SKU)
          .put(MERCHANT_SKU, FIELD_MERCHANT_SKU)
          .put(PRODUCT_NAME, FIELD_ITEM_NAME)
          .put(TOTAL_BARANG, FIELD_QUANTITY)
          .put(LOGISTIC_SERVICE, FIELD_LOGISTICS_PRODUCT_NAME)
          .put(MERCHANT_CODE, FIELD_MERCHANT_CODE)
          .put(STORE_NAME, FIELD_STORE_NAME)
          .put(ORDER_STATUS, FIELD_ORDER_ITEM_STATUS)
          .put(PICKUP_POINT, FIELD_PICKUP_POINT_NAME)
          .put(SHIPPING_STREET_ADDRESS, FIELD_SHIPPING_STREET_ADDRESS)
          .put(SHIPPING_CITY, FIELD_SHIPPING_CITY)
          .put(SHIPPING_PROVINCE, FIELD_SHIPPING_PROVINCE)
          .put(ORDER_ITEM_PRICE, FIELD_PRICE)
          .put(TOTAL_ORDER_ITEM_PRICE, FIELD_TOTAL_ORDER_ITEM_PRICE)
          .put(TOTAL_ORDER_PRICE, FIELD_TOTAL_ORDER_PRICE)
          .put(SELLER_DISCOUNT, FIELD_SELLER_DISCOUNT)
          .put(DISCOUNT_NAME, FIELD_DISCOUNT_NAME)
          .put(DISCOUNT_CODE, FIELD_DISCOUNT_CODE)
          .put(SELLER_VOUCHER, FIELD_SELLER_VOUCHER)
          .put(COMMISSION, FIELD_SELLER_COMMISSION)
          .put(MERCHANT_VOUCHER_NAME, FIELD_MERCHANT_VOUCHER_NAME)
          .put(MERCHANT_VOUCHER_CODE, FIELD_MERCHANT_VOUCHER_CODE)
          .put(SHIPPING_VOUCHER_NAME, FIELD_SHIPPING_VOUCHER_NAME)
          .put(SHIPPING_VOUCHER_CODE, FIELD_SHIPPING_VOUCHER_CODE)
          .put(NOTES_TO_SELLER, FIELD_ITEM_NOTES)
          .put(STATUS_D_UPDATED_TIMESTAMP, FIELD_STATUS_D_UPDATED_TIMESTAMP)
          .put(CANCELLED_BY, FIELD_CANCELLED_BY)
          .put(CANCELLATION_REASON, FIELD_CANCELLATION_REASON)
          .put(RMA_NUMBER, FIELD_RMA_NUMBER)
          .put(RETURN_STATUS, FIELD_RETURN_STATUS)
          .put(RETURN_REASON, FIELD_RETURN_REASON)
          .put(RETURN_RESOLUTION, FIELD_RETURN_ACTUAL_RESOLUTION)
          .build();

  private static final List<String> HEADER_LIST =
      new ArrayList<>(ONLINE_HEADER_TO_FIELD_MAP.keySet());

  private static final ImmutableMap<String, String> OFFLINE_HEADER_TO_FIELD_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(ORDER_NO, FIELD_ORDER_ID)
          .put(ORDER_ITEM_NO, FIELD_ORDER_ITEM_ID)
          .put(AWB_NO, FIELD_AWB_NUMBER)
          .put(TANGGAL_ORDER, FIELD_STATUS_FP_UPDATED_TIMESTAMP)
          .put(NAME_PEMESAN, FIELD_CUSTOMER_FULL_NAME)
          .put(SKU_CODE, FIELD_SKU_CODE)
          .put(BLIBLI_SKU, FIELD_ITEM_SKU)
          .put(MERCHANT_SKU, FIELD_MERCHANT_SKU)
          .put(PRODUCT_NAME, FIELD_ITEM_NAME)
          .put(TOTAL_BARANG, FIELD_QUANTITY)
          .put(PRODUCT_PRICE, FIELD_PRICE)
          .put(LOGISTIC_SERVICE, FIELD_LOGISTICS_PRODUCT_NAME)
          .put(MERCHANT_CODE, FIELD_MERCHANT_CODE)
          .put(STORE_NAME, FIELD_STORE_NAME)
          .put(ORDER_STATUS, FIELD_ORDER_ITEM_STATUS)
          .put(PICKUP_POINT, FIELD_PICKUP_POINT_NAME)
          .put(ORDER_TYPE, FIELD_ORDER_TYPE)
          .put(NOTES_TO_SELLER, FIELD_ITEM_NOTES)
          .put(KETERANGAN, FIELD_SALES_PERSON_NAME)
          .build();

  private static final List<String> OFFLINE_HEADER_LIST =
      new ArrayList<>(OFFLINE_HEADER_TO_FIELD_MAP.keySet());

  @Override
  public BulkCsvModel getCsvHeadersMap(BulkDownloadRequest request) {
    OrderDownloadRequest orderDownloadRequest = (OrderDownloadRequest) request;
    log.warn("show salesman info {}, request id : {}", orderDownloadRequest.isShowSalesmanInfo(), request.getRequestId());

    if (BooleanUtils.toBooleanDefaultIfNull(orderDownloadRequest.getOffline(), false)
        || OrderType.OFF2ON.getValue().equals(
        Optional.ofNullable(orderDownloadRequest.getOrderRequest().getOrderType())
            .flatMap(orderTypes -> orderTypes.stream().findFirst()).orElse(StringUtils.EMPTY))) {
      if (handoverFeatureSwitch) {
        return checkMerchantTypeAndGetHeaderMap(orderDownloadRequest);
      } else {
        return buildOfflineHeaderMap(orderDownloadRequest, false, false);
      }
    }

    SystemParameterConfig systemParameterConfig =
        this.systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.BULK_ORDER_DOWNLOAD_SOLR_ENABLED);
    boolean solrEnabled = Boolean.parseBoolean(systemParameterConfig.getValue());
    if (solrEnabled) {
      return new BulkCsvModel(SOLR_HEADER_LIST, ONLINE_SOLR_HEADER_TO_FIELD_MAP);
    }

    return new BulkCsvModel(HEADER_LIST, ONLINE_HEADER_TO_FIELD_MAP);
  }

  private BulkCsvModel checkMerchantTypeAndGetHeaderMap(OrderDownloadRequest orderDownloadRequest) {
    boolean addHandoverFields = false;
    
    try {
      ProfileResponse profileResponse = Optional.ofNullable(
          businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
              orderDownloadRequest.getMerchantId())).orElse(new ProfileResponse());
      if (profileResponse.isFbbActivated() && Optional.ofNullable(profileResponse.getCompany()).orElse(new CompanyDTO())
          .isCncActivated()) {
        addHandoverFields = true;
      }
    } catch (Exception e) {
      log.error("Error fetching business partner details for order download request: {}", orderDownloadRequest);
    }
    
    return buildOfflineHeaderMap(orderDownloadRequest, addHandoverFields, orderDownloadRequest.isShowPaymentMethodColumn());
  }

  private BulkCsvModel buildOfflineHeaderMap(OrderDownloadRequest orderDownloadRequest, 
                                             boolean addHandoverFields, 
                                             boolean addPaymentMethod) {
    List<String> headerList = new ArrayList<>(OFFLINE_HEADER_LIST);
    Map<String, String> headerToFieldMap = new HashMap<>(OFFLINE_HEADER_TO_FIELD_MAP);
    
    if (addHandoverFields) {
      headerList.add(ORDER_HANDED_OVER);
      headerList.add(IS_SUPERMARKETVERIFIED);
      headerToFieldMap.put(ORDER_HANDED_OVER, FIELD_HANDED_OVER);
      headerToFieldMap.put(IS_SUPERMARKETVERIFIED, FIELD_SUPERMARKET_VERIFIED);
    }
    
    if (addPaymentMethod) {
      headerList.add(PAYMENT_DISPLAY_NAME);
      headerToFieldMap.put(PAYMENT_DISPLAY_NAME, FIELD_PAYMENT_METHOD);
    }
    
    if (orderDownloadRequest.isShowSalesmanInfo()) {
      headerList.add(SALESMAN_NAME);
      headerList.add(SALESMAN_EMAIL);
      headerToFieldMap.put(SALESMAN_NAME, FIELD_SALESMAN_NAME);
      headerToFieldMap.put(SALESMAN_EMAIL, FIELD_SALESMAN_EMAIL);
    }
    
    return new BulkCsvModel(headerList, headerToFieldMap);
  }

  @Override
  public List<String> getHeaderList(BulkDataResponse bulkDataResponse) {
    //TODO : No Implementation for Now
    return Collections.emptyList();
  }

  @Override
  public Workbook modifyWorkbook(Workbook workbook, BulkDataResponse response) throws Exception {
    return null;
  }

  @Override
  public List<List<String>> getRowData(BulkDataResponse response) {
    return null;
  }

  @Override
  public String getDirectory(BulkDownloadRequest request) {
    return ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.ORDER) + request.getRequestId();
  }

  @Override
  public Map<String, Object> getEmailParams(BulkDownloadRequest request, String lang) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put("name", getUserName(request.getUsername()));
    emailParameters.put("reqId", request.getRequestId());
    emailParameters.put("businessPartnerCode", request.getMerchantId());
    emailParameters
        .put(EmailConstants.TEMPLATE_ID_PARAM, MessageUtil.getMessage(EmailConstants.ORDER_DOWNLOAD_TEMPLATE_ID, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, EmailConstants.ORDER_SUBJECT);
    return emailParameters;
  }

  @Override
  public int getRecordsUpdated(BulkDataResponse response) {
    BulkOrderResponse orderResponse = (BulkOrderResponse) response;
    return orderResponse.getResponseList().size();
  }

}

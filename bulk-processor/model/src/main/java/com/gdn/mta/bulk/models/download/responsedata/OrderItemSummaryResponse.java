package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class OrderItemSummaryResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -2521001267101244561L;
  private Boolean markForDelete = false;
  private String orderItemId;
  private String orderId;
  private Date createdDate;
  private String customerFullName;
  private String merchantSku;
  private String itemName;
  private Integer quantity;
  private Double price;
  private Double merchantAdjustment;
  private String logisticsProductCode;
  private String logisticsProductName;
  private String orderItemStatus;
  private String orderType;
  private String pickupPointCode;
  private String pickupPointName;
  private String productTypeCode;
  private String productTypeName;
  private String skuCode;
  private String itemSku;
  private String merchantCode;
  private String storeName;
  private Date endStoreClosingDate;
  private Date startStoreClosingDate;
  private Date autoCancelTimestamp;
  private Boolean lateFulfillment;
  private Boolean installationRequired;
  private Boolean isInternationalSeller;
  private String awbNumber;
  private String shippingInstruction;
  private String merchantDeliveryType;
  private String awbValidityStatus;
  private String merchantCommissionType;
  private String salesPersonName;
  private String salesmanEmail;
  private String salesmanName;
  private Date orderDate;
  private Date statusFPUpdatedTimestamp;
  private String logisticsOptionName;
  private String logisticsOptionCode;
  private Integer bookingHourStartInSecond;
  private Integer bookingHourEndInSecond;
  private Boolean isMerchantPaid;
  private String promoCombinationId;
  private String promoBundlingId;
  private String promoBundlingType;
  private String settlementCode;
  private boolean settlementCodeExpired;
  private boolean instantPickup;
  private String packageId;
  private Boolean packageCreated;
  private Boolean isDocumentSubmitted;
  private Boolean isVirtualNumberAllocated;
  private boolean isCashlessHandover;
  private Integer cashlessStatusUpdateSla;
  private Date fastFulfillmentTimestamp;
  private Date instantPickupAcceptOrderDeadline;
  private Boolean isShipBySeller;
  private boolean preOrder;
  private String preOrderType;
  private Integer preOrderValue;
  private Date preOrderDate;
  private Date printedAirwaybillDate;
  private Date printedShippingLabelDate;
  private Long pickupScheduleStartDate;
  private Long pickupScheduleEndDate;
  private String customerAddress;
  private String cancellationProcessStatus;
  private Integer itemInitialQuantity;
  private String customerId;
  private boolean mainSku;
  private String shippingAddress;
  private String customerConfirmationStatus;
  private Boolean isNotesToSeller;
  private String taxId;
  private String statusDescription;
  private String businessChannel;
  private String itemNotes;
  private String shippingStreetAddress;
  private String shippingCity;
  private String shippingProvince;
  private Double totalOrderItemPrice;
  private Double totalOrderPrice;
  private Double sellerDiscount;
  private String discountName;
  private String discountCode;
  private Double sellerVoucher;
  private Double sellerCommission;
  private String merchantVoucherName;
  private String merchantVoucherCode;
  private String shippingVoucherName;
  private String shippingVoucherCode;
  private Date statusDUpdatedTimestamp;
  private boolean handedOver = false;
  private String cancelledBy;
  private String cancellationReason;
  private String supermarketId;
  @JsonProperty("isSupermarketVerified")
  private boolean supermarketVerified;
  private String rmaNumber;
  private String returnActualResolution;
  private String returnStatus;
  private String returnReason;
  private String paymentMethod;
}

package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.domain.event.enums.ItemChangeEventType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDataChange extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1706784744332742626L;
  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String omniChannelSku;
  private String itemCode;
  private boolean isSynchronized;
  private String itemCatentryId;
  private MasterDataItem masterDataItem;
  private Boolean isLateFulfillment;
  private boolean off2OnChannelActive;
  private boolean isArchived;
  private PristineDataItemEventModel pristineDataItem;
  private boolean cncActivated;
  private String uniqueId;
  private List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
  private boolean isSubscribable;
  private boolean forceReview;
  private boolean isContentChanged;
  private String generatedItemName;
  private String mainImageUrl;
  private boolean markForDelete;
  private Boolean newData;
  private Set<String> preferredSubscriptionType = new HashSet<>();
  private Set<String> oldPreferredSubscriptionType = new HashSet<>();
  private boolean halalProduct;
  private String masterSku;
  private List<String> itemChangeEventTypesV2 = new ArrayList<>();
  private String brand;
  private String categoryCode;
  private Set<String> updatedFields = new HashSet<>();
  private String source;
  private String upcCode;
}

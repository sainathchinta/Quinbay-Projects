package com.gdn.mta.product.service;

import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.enums.FbbMigrationConstants;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;

@Service
@Slf4j
public class BackFillFbbFlagServiceImpl implements BackFillFbbFlagService {

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  private static final String HASH_CONSTANT = "#";

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void backFillFbbFlag(ProductFbbMigrationEventModel eventModel) {
    String businessPartnerId = eventModel.getIdentifier();
    log.info("Updating fbb flag for id {} ", businessPartnerId);
    try {
      productItemBusinessPartnerRepository.updateFbbFlagByStoreIdAndProductBusinessPartnerIdInAndPickupPointId(
        eventModel.getStoreId(), businessPartnerId,
        eventModel.getPickupPointCode(), true);
      productBusinessPartnerRepository.updateFbbFlagByStoreIdAndId(eventModel.getStoreId(),
        businessPartnerId, true);
    } catch (Exception ex) {
      log.error("Error updating fbb flag id {}", businessPartnerId, ex);
    }
  }

  @Override
  public void updateFbbPickupPoint(ProductFbbMigrationEventModel productFbbMigration) throws Exception {
    log.info("Changing pp code of {} to {}", productFbbMigration.getIdentifier(),
      productFbbMigration.getPickupPointCode());
    productLevel3Service.updatePickupPointCodes(toPickupPointUpdateRequest(productFbbMigration));
  }

  private PickupPointUpdateRequest toPickupPointUpdateRequest(
    ProductFbbMigrationEventModel productFbbMigration) {
    //identifier is saved as productSku#itemSku#ppCode
    String productSku = productFbbMigration.getIdentifier().substring(0,
      StringUtils.ordinalIndexOf(productFbbMigration.getIdentifier(), HASH_CONSTANT, 1));

    String itemSku = productFbbMigration.getIdentifier()
      .substring(StringUtils.ordinalIndexOf(productFbbMigration.getIdentifier(), HASH_CONSTANT,
          1)+1,
        StringUtils.ordinalIndexOf(productFbbMigration.getIdentifier(), HASH_CONSTANT, 2));

    String businessPartnerCode = productFbbMigration.getIdentifier().substring(
      StringUtils.ordinalIndexOf(productFbbMigration.getIdentifier(), HASH_CONSTANT, 2)+1);

    PickupPointUpdateRequest pickupPointUpdateRequest = new PickupPointUpdateRequest();
    PickupPointRequest pickupPointRequest = PickupPointRequest.builder().itemSku(itemSku)
      .pickupPointCode(productFbbMigration.getPickupPointCode()).build();
    pickupPointUpdateRequest.setProductSku(productSku);
    pickupPointUpdateRequest.setBusinessPartnerCode(businessPartnerCode);
    pickupPointUpdateRequest.setMarkDefaultAddress(false);
    pickupPointUpdateRequest.setDifferentLocation(false);
    pickupPointUpdateRequest.setNeedCorrection(
      !productFbbMigration.getProductState().equalsIgnoreCase(FbbMigrationConstants.ACTIVE.name()));
    pickupPointUpdateRequest.setItemsPickupPoint(Collections.singletonList(pickupPointRequest));
    pickupPointUpdateRequest.setFbbActivated(true);
    pickupPointUpdateRequest.setFbbMigration(true);
    return pickupPointUpdateRequest;
  }
}

package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.PickupPointRepository;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.service.api.PickupPointService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

@Service
public class PickupPointServiceImpl implements PickupPointService {

  public static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  public static final String USERNAME_MUST_NOT_BE_BLANK = "username must not be blank";
  public static final String PICKUP_POINT_MUST_NOT_BE_NULL = "pickup point must not be null";
  public static final String PICKUP_POINT_CODE_LIST_MUST_NOT_BE_NULL =
      "pickup point list must not be empty";

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Override
  public void upsertPickupPoint(String storeId, PickupPoint pickupPoint, String username) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(username), USERNAME_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(pickupPoint), PICKUP_POINT_MUST_NOT_BE_NULL);

    pickupPointRepository.upsertPickupPoint(storeId, pickupPoint, username);
  }

  @Override
  public List<PickupPoint> findPickupPointListByPickupPointCodeInAndCncActivatedFalse(
      String storeId, List<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes),
        PICKUP_POINT_CODE_LIST_MUST_NOT_BE_NULL);

    return pickupPointRepository
        .findByStoreIdAndPickupPointCodeInAndCncActivatedFalse(storeId, pickupPointCodes);
  }
}

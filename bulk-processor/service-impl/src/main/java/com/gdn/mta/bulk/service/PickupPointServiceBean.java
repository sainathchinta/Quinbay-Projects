package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
public class PickupPointServiceBean implements PickupPointService {

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${fetch.pickup.point.size}")
  private int fetchPickupPointSize;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Override
  public List<PickupPointResponse> getPickupPointSummaryFilter(int page,
    PickupPointFilterRequest request) throws ApplicationException {
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    Page<PickupPointResponse> pickupPointResponses;
    log.info("Fetching pp codes for bp code : {} ",request.getBusinessPartnerCode());
    request.setWaitingDeletion(RequestHelper.getWaitingDeletion(setWaitingDeletionForDeletePickupPoint));
    do {
      pickupPointResponses =
        businessPartnerRepository.filterBusinessPartnerPickupPointV2(page, fetchPickupPointSize, request);
      pickupPointResponseList.addAll(pickupPointResponses.getContent());
      page++;
    } while (page * fetchPickupPointSize < pickupPointResponses.getTotalElements());
    return pickupPointResponseList;
  }

  @Override
  public Page<PickupPointResponse> getSinglePickupPointSummaryFilter(int page, int size,
    PickupPointFilterRequest request) throws ApplicationException {
    log.info("Fetching single pp code for bp code : {}", request.getBusinessPartnerCode());
    request.setWaitingDeletion(
      RequestHelper.getWaitingDeletion(setWaitingDeletionForDeletePickupPoint));
    return businessPartnerRepository.filterBusinessPartnerPickupPointV2(page, size, request);
  }
}

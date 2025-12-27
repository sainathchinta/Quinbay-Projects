package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.data.domain.Page;

import java.util.List;

public interface PickupPointService {

  List<PickupPointResponse> getPickupPointSummaryFilter(int page, PickupPointFilterRequest request)
    throws ApplicationException;

  Page<PickupPointResponse> getSinglePickupPointSummaryFilter(int page, int size,
    PickupPointFilterRequest request) throws ApplicationException;
}

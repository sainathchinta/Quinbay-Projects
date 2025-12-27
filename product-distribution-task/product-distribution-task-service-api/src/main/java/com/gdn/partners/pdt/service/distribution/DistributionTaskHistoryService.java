package com.gdn.partners.pdt.service.distribution;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;

public interface DistributionTaskHistoryService {

  void create(List<ProductDistributionTask> distributionTasks) throws Exception;

}

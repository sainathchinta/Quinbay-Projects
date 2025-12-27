package com.gdn.x.mta.distributiontask.service.api;

public interface SlaCheckerService {

  /**
   * Send all SLA task that markfordelete false, state IN_REVIEW, to KAFKA
   *
   * @throws Exception
   */
  void execute() throws Exception;

  void turnTaskToDistributionList(String taskId) throws Exception;
}

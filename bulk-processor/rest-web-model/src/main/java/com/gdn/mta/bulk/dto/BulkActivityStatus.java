package com.gdn.mta.bulk.dto;

import static com.gdn.mta.bulk.entity.constants.BulkProcessConstant.DOWNLOAD_FAILED;
import static com.gdn.mta.bulk.entity.constants.BulkProcessConstant.DOWNLOAD_SUCCESS;
import com.gdn.mta.bulk.entity.BulkProcess;
import lombok.NoArgsConstructor;


@NoArgsConstructor
public enum BulkActivityStatus {
  SUCCESS,PARTIAL_SUCCESS,FAILED,IN_PROGRESS,PENDING,NA;

  public static BulkActivityStatus getActivityStatusFromBulkStatus(String bulkStatus) {
    BulkActivityStatus status = NA;
    switch (bulkStatus) {
      case BulkProcess.STATUS_FINISHED:
      case DOWNLOAD_SUCCESS: {
        status = SUCCESS;
        break;
      }
      case BulkProcess.STATUS_PARTIALLY_DONE: {
        status = PARTIAL_SUCCESS;
        break;
      }
      case BulkProcess.STATUS_IN_PROGRESS:
      case BulkProcess.STATUS_PROCESSED:
      case BulkProcess.STATUS_PUBLISHED:
      case BulkProcess.STATUS_IMAGE_PROCESSING:
      case BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1:
      case BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2:
      case BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING:
      case BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1:
      case BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2:{
        status = IN_PROGRESS;
        break;
      }
      case BulkProcess.STATUS_PENDING:
      case BulkProcess.STATUS_READY_TO_PROCESS:
        status = PENDING;
        break;
      case BulkProcess.STATUS_ABORTED:
      case BulkProcess.STATUS_FAIL:
      case DOWNLOAD_FAILED: {
        status = FAILED;
        break;
      }
    }
    return status;
  }
}
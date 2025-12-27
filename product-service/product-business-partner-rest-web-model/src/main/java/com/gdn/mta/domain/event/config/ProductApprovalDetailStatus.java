package com.gdn.mta.domain.event.config;

public enum ProductApprovalDetailStatus {

  MTA_Approve_Product_Request_Sent_PDT(1),

  PDT_Approve_Product_Request_Received_MTA(2),

  PDT_Approve_Product_Kafka_Event_Published_PBP(3),

  PBP_Approve_Product_Kafka_Event_Received_PDT(4),


  PBP_ApproveQC_Request_Sent_PBP(5),
  PBP_ApproveQC_Response_Received_PBP(6),


  PBP_UpdateProductContent_Request_Sent_PBP(7),
  PBP_UpdateProductContent_Request_Sent_PCB(8),
  PBP_UpdateProductContent_Response_Received_PCB(9),
  PBP_UpdateProductContent_Response_Received_PBP(10),

  PBP_ApproveContent_Request_Sent_PBP(11),
  PBP_ApproveContent_Response_Received_PBP(12),

  PBP_UpdateProductImage_Request_Sent_PBP(13),
  PBP_UpdateProductImage_Request_Sent_PCB(14),
  PBP_UpdateProductImage_Response_Received_PCB(15),
  PBP_UpdateProductImage_Response_Received_PBP(16),

  PBP_ApproveImage_Request_Sent_PBP(17),
  PBP_ApproveImage_Response_Received_PBP(18),

  PBP_Process_Image_Kafka_Event_Publish_From_Distribution_QC_Task_Service_MTA(19),
  MTA_Process_Image_Kafka_Event_Received_PBP(20),

  MTA_ProcessImage_Request_Sent_XGP(21),
  MTA_Image_Processed_Kafka_Event_Received_XGP(22),

  MTA_ApproveImage_Kafka_Event_PUBLISHED_PBP(23),
  PBP_ApproveImage_Kafka_Event_Received_MTA(24),

  PBP_UpdateImageName_Request_Sent_PCB(25),
  PBP_UpdateImageName_Response_Received_PCB(26),

  PBP_ApproveFinalImage_Request_Sent_PBP(27),
  PBP_ApproveFinalImage_Response_Received_PBP(28),

  PBP_ImageApproved_Kafka_Event_Published_MTA(29),
  MTA_ImageApproved_Kafka_Event_Received_PBP(30),

  MTA_Remove_Product_From_PDT_Request_Sent_PDT(31),
  PDT_Remove_Product_From_PDT_Request_RECEIVED_MTA(32),
  PDT_Remove_Product_From_PDT_Request_COMPLETED_MTA(33),
  MTA_Remove_Product_From_PDT_Response_Received_PDT(34),

  MTA_Remove_Product_From_PDT_After_Image_Approval_Request_Sent_PDT(35),
  MTA_Remove_Product_From_PDT_After_Image_Approval_Response_Received_PDT(36),

  MTA_Remove_Product_From_PDT_After_Graphics_Processor_Request_Sent_PDT(37),
  MTA_Remove_Product_From_PDT_After_Graphics_Processor_Response_Received_PDT(38),

  MTA_Remove_Product_From_PDT_After_Process_Image_Request_Sent_PDT(39),
  MTA_Remove_Product_From_PDT_After_Process_Image_Response_Received_PDT(40),

  MTA_Graphics_Process_Kafka_Message_Received_XGP(41),

  MTA_Approve_Image_From_Graphics_Processor_Request_Sent_PBP(42),
  MTA_Approve_Image_From_Graphics_Processor_Response_Received_PBP(43),

  MTA_Move_Fail_Product_To_QC_From_Graphics_Processor_Request_Sent_PDT(44),
  MTA_Move_Fail_Product_To_QC_From_Graphics_Processor_Response_Received_PDT(45),

  MTA_Move_Fail_Product_To_QC_From_Image_Kafka_Subscriber_Request_Sent_PDT(46),
  MTA_Move_Fail_Product_To_QC_From_Image_Kafka_Subscriber_Response_Received_PDT(47),

  MTA_Move_Fail_Product_To_QC_From_Process_Image_Subscriber_Request_Sent_PDT(48),
  MTA_Move_Fail_Product_To_QC_From_Process_Image_Subscriber_Response_Received_PDT(49),

  MTA_Reject_Process_Image_From_Process_Image_Subscriber_Request_Sent_PBP(50),
  MTA_Reject_Process_Image_From_Process_Image_Subscriber_Response_Received_PBP(51),

  MTA_Reject_Process_Image_From_Image_Kafka_Subscriber_Request_Sent_PBP(52),
  MTA_Reject_Process_Image_From_Image_Kafka_Subscriber_Response_Received_PBP(53),
  PBP_Process_Image_Kafka_Event_Publish_From_Distribution_Task_Consumer_MTA(54),

  MTA_Retry_Approve_Product_Kafka_Sent_PBP(56),
  PDT_Move_Failed_Task_To_QC_Request_Received_MTA(57),
  PDT_Move_Failed_Task_To_QC_Response_Sent_MTA(58),
  MTA_Move_Failed_Task_To_QC_Response_Received_PDT(59),

  PBP_Retry_ApproveFinalImage_Request_Sent_PBP(60),
  PBP_Retry_ApproveFinalImage_Response_Received_PBP(61),

  MTA_ApproveFinal_Image_From_Graphic_processor_Request_Sent_PBP(62),
  MTA_ApproveFinal_Image_From_Graphic_processor_Response_Received_PBP(63),

  PBP_ApproveFinal_Image_Request_Received_MTA(64),
  PBP_ApproveFinal_Image_Response_Sent_MTA(65),
  PBP_Reject_Process_Image_Request_Received_MTA(66),
  PBP_Reject_Process_Image_Response_Sent_MTA(67),

  PDT_Approve_Product_Kafka_Event_Published_MTA(68),

  MTA_Retry_ApproveFinal_Image_From_Graphic_processor_Request_Sent_PBP(69),
  MTA_Retry_ApproveFinal_Image_From_Graphic_processor_Response_Received_PBP(70);

  private Integer event;

  ProductApprovalDetailStatus(Integer event){
    this.event = event;
  }

  public Integer getEvent() {
    return event;
  }
}

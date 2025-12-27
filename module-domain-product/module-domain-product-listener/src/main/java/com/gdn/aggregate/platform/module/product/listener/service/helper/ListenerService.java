package com.gdn.aggregate.platform.module.product.listener.service.helper;

import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.DeleteExpiredEvent;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.MainService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.DataUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.event.ProductElasticSearchDeletionEvent;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductSaved;
import com.gdn.aggregate.platform.module.product.listener.model.raw.BusinessPartnerProfileChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductRemoved;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarSearchLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.DenpasarShippingLogisticOptionChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FbbChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryInfoChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryStatusChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.ProductReviewChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StoreClosingMerchantChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.TradeInChangeEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateQueue;
import com.gdn.aggregate.platform.module.product.listener.model.util.PickupPointEventType;
import com.gdn.aggregate.platform.module.product.listener.model.util.TerminatedSellerDeletionEventModel;
import com.gdn.aggregate.platform.module.product.listener.properties.BatchKafkaProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.EventConsumptionProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.PublisherProperties;
import com.gdn.aggregate.platform.module.product.listener.properties.TimeStampProperties;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.updater.MainUpdater;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.AdjustmentProductQuotaService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.CheapestPriceDayService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.MerchantDiscountPriceService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.PickupPointInventoryService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.ProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.TagService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.raw.UpdateQueueService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.ProductDeletionService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveProcessedService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveRawService;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveRawServiceV2;
import com.gdn.aggregate.platform.module.product.listener.service.saver.SaveSemiService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaCampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleGroupService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleScheduleService;
import com.gdn.aggregate.platform.module.product.listener.util.DataConverter;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import io.micrometer.tracing.Tracer;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.Comparator;

@Component("ProductListenerService")
@Slf4j
public class ListenerService extends DataUtil {

  private static final String COMMA = ",";
  public static final String HANDLE_FLASHSALE_PRODUCT_FROM_ADJUSTMENT =
    "handleFlashsaleProductFromAdjustment";
  public static final String HANDLE_REGULAR_CAMPAIGN_FROM_ADJUSTMENT =
    "handleRegularCampaignFromAdjustment";

  private SchedulerHelper schedulerHelper;

    private UpdateQueueService updateQueueService;

    private AdjustmentProductQuotaService adjustmentProductQuotaService;

    private CampaignProductService campaignProductService;

    private CheapestPriceDayService cheapestPriceDayService;

    private MerchantDiscountPriceService merchantDiscountPriceService;

    private ProductService productService;

    private ItemService itemService;

    private PickupPointService pickupPointService;

    private TagService tagService;

    private SivaProductService sivaProductService;

    private SivaItemService sivaItemService;

    private SivaCampaignProductService sivaCampaignProductService;

    private SivaFlashsaleScheduleService sivaFlashsaleScheduleService;

    private SivaFlashsaleGroupService sivaFlashsaleGroupService;

    private SaveRawService saveRawService;

    private SaveSemiService saveSemiService;

    private SaveProcessedService saveProcessedService;

    private MainService mainService;

    private MainUpdater mainUpdater;

    private PublisherService publisherService;

    private SchedulerService schedulerService;

    private SaveRawServiceV2 saveRawServiceV2;

    private EventConsumptionProperties eventConsumptionProperties;

    private DataConverter dataConverter;

    private ProductDeletionService productDeletionService;

    private PickupPointInventoryService pickupPointInventoryService;

    public static final String SOURCE_PRODUCT_PUBLISH = "PRODUCT_PUBLISH";

    @Value("${skip.product.publish.l4.event}")
    private boolean skipProductPublishL4Event = false;

    @Value("${pickup.point.change.queue.enabled}")
    private boolean newItemPickupPointUpsertEventEnabled;

    @Value("${new.inventory.info.change.queue.enabled}")
    private boolean newInventoryInfoEventEnabled;

    @Value("${new.raw.item.data.change.queue.enabled}")
    private boolean newRawItemUpdateQueueEnabled;

    @Value("${module.domain.product.raw.product.combined.upsert}")
    private boolean rawProductUpsertCombinedEventEnabled;

    @Value("${new.master.data.flow.enabled}")
    private boolean newMasterDataFlowEnabled;

    @Value("${delete.product.data.on.termination}")
    private boolean deleteProductDataOnTermination;

    @Value("${change.types.to.skip.item.and.pickup.point.event}")
    private String changeTypesToSkipItemAndPickupPointEvent;

    @Value("${module.domain.product.adjustment.handles.campaigns}")
    private boolean adjustmentHandlesCampaigns;

    @Value("${module.domain.product.flashsale.promo.types}")
    private String flashsalePromoTypesString;

    @Autowired
    private PublisherProperties publisherProperties;

    private List<String> getFlashsalePromoTypes() {
        return Arrays.asList(flashsalePromoTypesString.split(","));
    }

    @Autowired
    private Tracer tracer;

    @Autowired
    private TimeStampProperties timeStampProperties;

    @Autowired
    private BatchKafkaProperties batchKafkaProperties;

    private final String STOCK_UPDATE_EVENT_COMMAND = "onInventoryStockUpdateEvent";
    private final String STOCK_REPUBLISH_EVENT_COMMAND = "onInventoryStockRepublishEvent";

    @Autowired
    public ListenerService(ObjectMapper objectMapper, TimeService timeService, SchedulerHelper schedulerHelper,
        UpdateQueueService updateQueueService, AdjustmentProductQuotaService adjustmentProductQuotaService,
        CampaignProductService campaignProductService, CheapestPriceDayService cheapestPriceDayService,
        MerchantDiscountPriceService merchantDiscountPriceService,
        ProductService productService, ItemService itemService, PickupPointService pickupPointService,
        TagService tagService, SivaCampaignProductService sivaCampaignProductService,
        SivaProductService sivaProductService, SivaItemService sivaItemService,
        SivaFlashsaleScheduleService sivaFlashsaleScheduleService, SivaFlashsaleGroupService sivaFlashsaleGroupService,
        SaveRawService saveRawService, SaveSemiService saveSemiService, SaveProcessedService saveProcessedService,
        MainService mainService, MainUpdater mainUpdater, PublisherService publisherService,
        SchedulerService schedulerService, SaveRawServiceV2 saveRawServiceV2,
        EventConsumptionProperties eventConsumptionProperties, DataConverter dataConverter,
        ProductDeletionService productDeletionService, PickupPointInventoryService pickupPointInventoryService) {
      super(objectMapper, timeService);
      this.schedulerHelper = schedulerHelper;
        this.updateQueueService = updateQueueService;
        this.adjustmentProductQuotaService = adjustmentProductQuotaService;
        this.campaignProductService = campaignProductService;
        this.cheapestPriceDayService = cheapestPriceDayService;
        this.merchantDiscountPriceService = merchantDiscountPriceService;
        this.productService = productService;
        this.itemService = itemService;
        this.pickupPointService = pickupPointService;
        this.tagService = tagService;
        this.sivaProductService = sivaProductService;
        this.sivaItemService = sivaItemService;
        this.sivaCampaignProductService = sivaCampaignProductService;
        this.sivaFlashsaleScheduleService = sivaFlashsaleScheduleService;
        this.sivaFlashsaleGroupService = sivaFlashsaleGroupService;
        this.saveRawService = saveRawService;
        this.saveSemiService = saveSemiService;
        this.saveProcessedService = saveProcessedService;
        this.mainService = mainService;
        this.mainUpdater = mainUpdater;
        this.publisherService = publisherService;
        this.schedulerService = schedulerService;
        this.saveRawServiceV2 = saveRawServiceV2;
        this.eventConsumptionProperties = eventConsumptionProperties;
        this.dataConverter = dataConverter;
        this.productDeletionService = productDeletionService;
        this.pickupPointInventoryService = pickupPointInventoryService;
    }

    public void onUpdateQueueEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, int level, String traceId) {
        IdTimestamp event = toData(record,IdTimestamp.class);
        List<UpdateQueue> updateQueues = updateQueueService.getUpdateQueueByLevel(level,event.getTimestamp());
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                   timeStampProperties.considerTimeStamp(Topics.UPDATE_QUEUE_DENPASAR, false), traceId);
        Optional.ofNullable(saveParam)
            .map(SaveParam::getDbParam)
            .ifPresent(val -> val.setLevel(level));
        String command = String.format("onUpdateQueueEventL%s",level);
        LoggerUtil.sendSuccessListenLog(event,command,record.topic(), traceId);

        Mono<Boolean> updateQueueCommand;
        if (CollectionUtils.isEmpty(updateQueues)) {
            updateQueueCommand = MainUtil.successResult();
        } else {
            Flux<UpdateQueue> flux = Flux.fromIterable(updateQueues);

            Function<UpdateQueue, Mono<Boolean>> processor = updateQueue ->
              Mono.just(updateQueue)
                .map(uq -> updateQueueService.getItemByUpdateQueue(uq, event.getTimestamp()))
                .flatMap(item -> mainUpdater.queueUpdateSivaBothByItem(item, saveParam));

            if (publisherProperties.isThrottlingEnabled()) {
                updateQueueCommand = flux
                  .flatMap(processor, publisherProperties.getConcurrency())
                  .reduce(MainUtil::reduce)
                  .flatMap(success -> publisherService.publishUpdateQueue(event, saveParam));
            } else {
                updateQueueCommand = flux
                  .flatMap(processor)
                  .reduce(MainUtil::reduce)
                  .flatMap(success -> publisherService.publishUpdateQueue(event, saveParam));
            }
        }

        updateQueueCommand
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("QueueEvent"))
            .subscribe(
                s -> LoggerUtil.success(command, traceId),
                t -> LoggerUtil.error(t,command, traceId)
            );
    }

    public void fullReconstructByProductSku(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        Optional.ofNullable(toData(record,IdTimestamp.class))
            .map(IdTimestamp::getId)
            .map(pickupPointService::getMinPickupPointByProductSku)
            .ifPresent(pickupPoint -> doOnPickupPointEvent(pickupPoint, groupId, record.topic(), migration, true, "fullReconstructByProductSku",traceId));
    }

    public void fullReconstructByItemSku(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        Optional.ofNullable(toData(record,IdTimestamp.class))
            .map(IdTimestamp::getId)
            .map(pickupPointService::getAnyPickupPointByItemSku)
            .ifPresent(pickupPoint -> doOnPickupPointEvent(pickupPoint, groupId, record.topic(), migration, true, "fullReconstructByItemSku", traceId));
    }

    public void onAdjustmentProductEventV2(ConsumerRecord<String, String> record, String groupId, boolean migration,
        Integer expectedPriority, String traceId) {
        try {
            Optional.ofNullable(getObjectMapper().readValue(record.value(), AdjustmentProduct.class)).ifPresent(
                event -> doOnAdjustmentProductEvent(event, record, groupId, migration, expectedPriority, traceId));
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming adjustment product event record : {} ", record, e);
        }
    }

    public void doOnAdjustmentProductEvent(AdjustmentProduct event, ConsumerRecord<String, String> record,
        String groupId, boolean migration, Integer expectedPriority, String traceId) {
        if (!ModuleProductUtil.priorityExpected(expectedPriority, event.getPriority()))
            return;
        String campaignType = ModuleProductUtil.getCampaignType(event.getPriority());
        String command = String.format("onAdjustmentProductEvent : %s", campaignType);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.ADJUSTMENT_PRODUCT, false), traceId);
        LoggerUtil.sendSuccessListenLog(event, command, record.topic(), traceId);
        if (isEligibleForAdjustmentProductEventProcessing(event)) {
            if (isEligibleForAdjustmentProductEventProcessingV2(event)) {
              saveRawServiceV2.saveAdjustmentProduct(event, saveParam);
              if (adjustmentHandlesCampaigns && ModuleProductUtil.isPromoCampaign(event)) {
                handleCampaignProductFromAdjustment(event, saveParam, traceId);
              }
            } else {
                onAdjustmentProductEvent(record, groupId, migration, expectedPriority, traceId);
            }
        }
    }

    public void onAdjustmentProductEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, Integer expectedPriority, String traceId) {
        AdjustmentProduct event = toData(record,AdjustmentProduct.class);
        if (!ModuleProductUtil.priorityExpected(expectedPriority,event.getPriority())) return;
        if (isFlashSaleAdjustmentProductV2EventBlackListedSellers(event)) return;
        String campaignType = ModuleProductUtil.getCampaignType(event.getPriority());
        String command = String.format("onAdjustmentProductEvent : %s",campaignType);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.ADJUSTMENT_PRODUCT, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,command,record.topic(), traceId);
        saveRawService.saveAdjustmentProduct(event,saveParam)
                .onErrorResume(MainUtil::errorResult)
                .subscribeOn(schedulerHelper.of("AdjustmentProduct"))
                .subscribe(
                        s -> LoggerUtil.success(command,traceId),
                        t -> LoggerUtil.error(t,command,traceId)
                );
    }

    private boolean isFlashSaleAdjustmentProductV2EventBlackListedSellers(AdjustmentProduct adjustmentProduct) {
        return Optional.ofNullable(eventConsumptionProperties.getFlashSaleAdjustmentProductV2EventBlackListedSellers())
            .orElseGet(ArrayList::new).stream().anyMatch(bpCode -> adjustmentProduct.getItemSku().startsWith(bpCode));
    }

    private boolean isEligibleForAdjustmentProductEventProcessingV2(AdjustmentProduct adjustmentProduct) {
        return eventConsumptionProperties.isAdjustmentProductV2EventEnabled() && Optional.ofNullable(
                eventConsumptionProperties.getAdjustmentProductV2EventBlackListedSellers()).orElseGet(ArrayList::new)
            .stream().noneMatch(val -> adjustmentProduct.getItemSku().startsWith(val));
    }

    private boolean isEligibleForAdjustmentProductEventProcessing(AdjustmentProduct adjustmentProduct) {
        return Optional.ofNullable(
                eventConsumptionProperties.getAdjustmentProductEventBlackListedSellers()).orElseGet(ArrayList::new)
            .stream().noneMatch(val -> adjustmentProduct.getItemSku().startsWith(val));
    }


    private void handleCampaignProductFromAdjustment(AdjustmentProduct adjustmentProduct, SaveParam saveParam, String traceId) {
        try {
            if (ModuleProductUtil.isFlashsalePromoType(adjustmentProduct.getPromoType(), getFlashsalePromoTypes())) {
                handleFlashsaleProductFromAdjustment(adjustmentProduct, saveParam, traceId);
            } else {
                handleRegularCampaignFromAdjustment(adjustmentProduct, saveParam, traceId);
            }
        } catch (Exception e) {
            log.error("Error handling campaign product from adjustment: {}", adjustmentProduct.toId(), e);
            TraceHelper.recordError(tracer, e);
        }
    }

    private void handleFlashsaleProductFromAdjustment(AdjustmentProduct adjustmentProduct, SaveParam saveParam, String traceId) {
        FlashsaleProduct flashsaleProduct = ModuleProductUtil.fromAdjustmentProductToFlashsaleProduct(adjustmentProduct, getFlashsalePromoTypes());
        if (flashsaleProduct != null) {
            log.info("Creating/updating flashsale product from adjustment: {}", adjustmentProduct.getCampaignCode());
            saveRawService.saveFlashsaleProduct(flashsaleProduct, saveParam)
                .onErrorResume(MainUtil::errorResult)
                .subscribe(
                    s -> LoggerUtil.success(HANDLE_FLASHSALE_PRODUCT_FROM_ADJUSTMENT, traceId),
                    t -> LoggerUtil.error(t, HANDLE_FLASHSALE_PRODUCT_FROM_ADJUSTMENT, traceId)
                );
        }
    }

    private void handleRegularCampaignFromAdjustment(AdjustmentProduct adjustmentProduct, SaveParam saveParam, String traceId) {
        CampaignProduct campaignProduct = ModuleProductUtil.fromAdjustmentProductToCampaignProduct(adjustmentProduct, getFlashsalePromoTypes());
        if (campaignProduct != null) {
            List<CampaignProduct> campaignProducts = MainUtil.toList(campaignProduct);
            List<CampaignProduct> filteredProducts = checkAndFilterBlackListCampaignProducts(campaignProducts);

            if (CollectionUtils.isNotEmpty(filteredProducts)) {
                saveRawService.saveCampaignProducts(filteredProducts, saveParam)
                    .onErrorResume(MainUtil::errorResult)
                    .subscribe(
                        s -> LoggerUtil.success(HANDLE_REGULAR_CAMPAIGN_FROM_ADJUSTMENT, traceId),
                        t -> LoggerUtil.error(t, HANDLE_REGULAR_CAMPAIGN_FROM_ADJUSTMENT, traceId)
                    );
            }
        }
    }

    public void onAdjustmentProductQuotaEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, Integer expectedPriority, String traceId) {
        AdjustmentProductQuota event = toData(record,AdjustmentProductQuota.class);
        Optional.of(event)
            .map(AdjustmentProductQuota::getAdjustmentProductId)
            .map(adjustmentProductQuotaService::getExistingAdjustmentProductQuota)
            .map(AdjustmentProductQuota::getPriority)
            .ifPresent(event::setPriority);
        if (!ModuleProductUtil.priorityExpected(expectedPriority,event.getPriority())) return;
        String campaignType = ModuleProductUtil.getCampaignType(event.getPriority());
        String command = String.format("onAdjustmentProductQuotaEvent : %s",campaignType);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.ADJUSTMENT_PRODUCT_QUOTA, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,command,record.topic(),traceId);

        saveRawService.saveAdjustmentProductQuota(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("AdjustmentProductQuota"))
            .subscribe(
                s -> LoggerUtil.success(command,traceId),
                t -> LoggerUtil.error(t,command,traceId)
            );
    }

    public void onAdjustmentProductSavedEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, Integer expectedPriority, String traceId) {
        AdjustmentProductSaved event = toData(record,AdjustmentProductSaved.class);
        if(isEligibleForAdjustmentProductSavedEventProcessing(event)) return;
        if (!ModuleProductUtil.priorityExpected(expectedPriority,event.getPriority())) return;
        String campaignType = ModuleProductUtil.getCampaignType(event.getPriority());
        String command = String.format("onAdjustmentProductSavedEvent : %s",campaignType);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.ADJUSTMENT_PRODUCT_SAVED, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,command,record.topic(),traceId);

        Mono.fromCallable(() -> adjustmentProductQuotaService.getExistingAdjustmentProductQuota(event))
            .flatMap(adjustmentProductQuota -> saveRawService.saveAdjustmentProductQuota(adjustmentProductQuota,saveParam))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("AdjustmentProductSaved"))
            .subscribe(
                s -> LoggerUtil.success(command,traceId),
                t -> LoggerUtil.error(t,command,traceId)
            );
    }

    private boolean isEligibleForAdjustmentProductSavedEventProcessing(AdjustmentProductSaved adjustmentProductSaved) {
        return Optional.ofNullable(
                eventConsumptionProperties.getPromotionAdjustmentProductSaveEventBlackListedSellers())
            .orElseGet(ArrayList::new).stream()
            .anyMatch(bpCode -> adjustmentProductSaved.getItemSku().startsWith(bpCode));
    }

    public void onInventoryInfoChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        InventoryInfoChange event =
            dataConverter.toInventoryInfoChangeFromCombinedUpsertEventModel(record, newInventoryInfoEventEnabled);
        Optional.ofNullable(event)
            .map(val -> ModuleProductUtil.toPickupPointId(val.getItemSku(),val.getPickupPointCode()))
            .map(pickupPointService::getExistingPickupPoint)
            .map(pickupPoint -> {
                pickupPoint.setTimestamp(event.getTimestamp());
                return pickupPoint;
            })
            .ifPresent(pickupPoint -> doOnPickupPointEvent(pickupPoint, groupId, record.topic(), false, false, "onInventoryInfoChangeEvent", traceId));
    }

    public void onInventoryStatusChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        InventoryStatusChange event = toData(record,InventoryStatusChange.class);

        Optional.ofNullable(event)
            .map(val -> ModuleProductUtil.toPickupPointId(val.getItemSku(),val.getPickupPointCode()))
            .map(pickupPointService::getExistingPickupPoint)
            .map(pickupPoint -> {
                pickupPoint.setTimestamp(event.getTimestamp());
                return pickupPoint;
            })
            .ifPresent(pickupPoint -> doOnPickupPointEvent(pickupPoint, groupId, record.topic(), false, false, "onInventoryStatusChangeEvent",traceId));
    }

    public void onMerchantDiscountPriceChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        MerchantDiscountPrice event = toData(record, MerchantDiscountPrice.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.MERCHANT_DISCOUNT_PRICE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onMerchantDiscountPriceChangeEvent",record.topic(),traceId);

        saveRawService.saveMerchantDiscountPriceChange(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("MerchantDiscountPrice"))
            .subscribe(
                s -> LoggerUtil.success("onMerchantDiscountPriceChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onMerchantDiscountPriceChangeEvent",traceId)
            );
    }

    public void onProductReviewChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        ProductReviewChangeEvent event = toData(record,ProductReviewChangeEvent.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.PRODUCT_REVIEW_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onProductReviewChangeEvent",record.topic(),traceId);

        mainUpdater.directUpdateSivaBothByProductReviewChangeEvent(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("ProductReview"))
            .subscribe(
                s -> LoggerUtil.success("onProductReviewChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onProductReviewChangeEvent",traceId)
            );
    }

    public void onStoreClosingMerchantChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        StoreClosingMerchantChangeEvent event = toData(record,StoreClosingMerchantChangeEvent.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.STORE_CLOSING_MERCHANT_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onStoreClosingMerchantChangeEvent",record.topic(),traceId);

        mainUpdater.directUpdateSivaBothByStoreClosingMerchantChangeEvent(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("StoreClosingMerchantChange"))
            .subscribe(
                s -> LoggerUtil.success("onStoreClosingMerchantChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onStoreClosingMerchantChangeEvent",traceId)
            );
    }

    public void onMasterDataEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        MasterData event = toData(record, MasterData.class);
        if (deleteProductDataOnTermination && event.isProductMarkForDelete()) {
            // Drop event if product is Mark for Delete , only happening is case of Product
            // rejection and seller termination
            return;
        }
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.MASTER_DATA, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onMasterDataEvent",record.topic(),traceId);
        saveRawService.saveMasterData(event, saveParam, newMasterDataFlowEnabled)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("MasterData"))
            .subscribe(
                s -> LoggerUtil.success("onMasterDataEvent", traceId),
                t -> LoggerUtil.error(t,"onMasterDataEvent", traceId)
            );
    }

    public void onProductEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        Product event = toData(record, Product.class);
        if (ModuleProductUtil.isProductRejected(event.getProductChangeEventType(), traceId,
          event.getProductSku(), Collections.PRODUCT, deleteProductDataOnTermination,
          getChangeTypesToSkipL4AndL5Events())) {
            productDeletionService.performDeletionFromAllDataSources(
              List.of(ModuleProductUtil.toTerminatedSellerDeletionEventModel(event)), traceId);
            return;
        }
        SaveParam saveParam = buildSaveParam(event, record, groupId, migration, traceId);

        LoggerUtil.sendSuccessListenLog(event, "onProductEvent", record.topic(), traceId);

        if (rawProductUpsertCombinedEventEnabled) {
            publishRawProductUpsert(saveParam, event, traceId);
        } else {
            handleDirectRawProductUpsert(saveParam, event, traceId);
        }
    }

    private SaveParam buildSaveParam(Product event, ConsumerRecord<String, String> record,
      String groupId, boolean migration, String traceId) {
        return ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(),
          migration, timeStampProperties.considerTimeStamp(Topics.PRODUCT, false), traceId);
    }

    private void publishRawProductUpsert(SaveParam saveParam, Product event, String traceId) {
        saveRawServiceV2.publishCombinedRawProductUpsertModel(saveParam, event.getProductSku(),
          Topics.PRODUCT, event);
        LoggerUtil.success("Raw product upsert published successfully", event.getProductSku());
    }

    private void handleDirectRawProductUpsert(SaveParam saveParam, Product event, String traceId) {
        saveRawService.saveProduct(event, saveParam, false).onErrorResume(MainUtil::errorResult)
          .subscribeOn(schedulerHelper.of("Product"))
          .doOnSuccess(s -> LoggerUtil.success("Product event processed successfully", traceId))
          .doOnError(t -> LoggerUtil.error(t, "Error processing product event", traceId))
          .subscribe();
    }


    public void onItemEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        Optional.ofNullable(toData(record, Item.class))
            .ifPresent(newItem -> {
                doOnItemEvent(newItem,groupId,record.topic(),migration,"onItemEvent",traceId);
            });
    }

    public void onItemEventV2(ConsumerRecord<String, String> record, String groupId, boolean migration,
        String traceId) {
        try {
            Optional.ofNullable(dataConverter.getItemFromItemUpdateEventModel(record, newRawItemUpdateQueueEnabled))
                .ifPresent(newItem -> {
                if (isEligibleForItemDataChangeEventProcessing(newItem)) {
                    if (isEligibleForItemDataChangeEventProcessingV2(newItem)) {
                        doOnItemEventV2(record, groupId, migration, traceId, newItem);
                    } else {
                        onItemEvent(record, groupId, migration, traceId);
                    }
                }
            });
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming l4 event record : {} ", record, e);
        }
    }

    public void onNewItemEventV2(ConsumerRecord<String, String> record, String groupId, String traceId) {
        try {
            boolean migration = newRawItemUpdateQueueEnabled && getObjectMapper().readValue(record.value(),
                RawItemCombinedUpsertEventModel.class).isMigration();
            onItemEventV2(record, groupId, migration, traceId);
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming l4 event record : {} ", record, e);
        }
    }

    public void processSivaItemUpdateQueueEvent(ConsumerRecord<String, String> record) throws JsonProcessingException {
        try {
            SivaItemCombinedUpsertEventModel sivaItemCombinedUpsertEventModel =
                getObjectMapper().readValue(record.value(), SivaItemCombinedUpsertEventModel.class);
            saveRawServiceV2.directUpdateSivaItemByItem(sivaItemCombinedUpsertEventModel.getItem(),
              sivaItemCombinedUpsertEventModel.getSaveParam(), null,
              sivaItemCombinedUpsertEventModel.getSivaItem(),
              sivaItemCombinedUpsertEventModel.isDirectSave(),
              sivaItemCombinedUpsertEventModel.getEligibleDataSourcesForUpsert());
        } catch (Exception exception) {
            TraceHelper.recordError(tracer,exception);
            log.error("Error while processing new Siva Item Update Queue ", exception);
        }
    }

    public void processSivaItemUpdateQueueEventBatch(List<ConsumerRecord<String, String>> records) {
        try {
            Map<String, ConsumerRecord<String, String>> deduplicatedRecords = new HashMap<>();
            Map<String, Integer> skipCounts = new HashMap<>();

            deduplicateRecords(records, deduplicatedRecords, skipCounts);
            int totalSkipped = skipCounts.values().stream().mapToInt(Integer::intValue).sum();
            processDeduplicatedRecords(deduplicatedRecords);
            if (batchKafkaProperties.isDebugEnabled()) {
                logDataForDeduplication(records, deduplicatedRecords, totalSkipped, skipCounts);
            }

        } catch (Exception exception) {
            TraceHelper.recordError(tracer, exception);
            log.error("Error while processing batch of Siva Item Update Queue events", exception);
        }
    }

    private static void logDataForDeduplication(List<ConsumerRecord<String, String>> records,
      Map<String, ConsumerRecord<String, String>> deduplicatedRecords, int totalSkipped,
      Map<String, Integer> skipCounts) {
        log.info(
          "Batch processing: original {} records, deduplicated to {} unique itemSkus, total skipped records: {}",
          records.size(), deduplicatedRecords.size(), totalSkipped);
        skipCounts.forEach(
          (sku, count) -> log.info("Skipped {} record(s) for itemSku={}", count, sku));
    }

    private void deduplicateRecords(List<ConsumerRecord<String, String>> records,
      Map<String, ConsumerRecord<String, String>> deduplicatedRecords, Map<String, Integer> skipCounts) {
        Map<String, List<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>>>
          groupedBySku =
          records.stream().map(record -> ModuleProductUtil.parseToEntry(record, getObjectMapper()))
            .filter(Optional::isPresent).map(Optional::get)
            .filter(entry -> Objects.nonNull(ModuleProductUtil.extractItemSku(entry.getValue())))
            .collect(
              Collectors.groupingBy(entry -> ModuleProductUtil.extractItemSku(entry.getValue())));

      for (Map.Entry<String, List<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>>> data : groupedBySku.entrySet()) {
        String sku = data.getKey();
        List<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>> entries = data.getValue();
        if (entries.size() > 1) {
          skipCounts.put(sku, entries.size() - 1);
        }
        entries.stream().max(Comparator.comparing(entry -> entry.getValue().getTimestamp()))
          .ifPresent(latest -> deduplicatedRecords.put(sku, latest.getKey()));
      }
    }

    private void processDeduplicatedRecords(Map<String, ConsumerRecord<String, String>> deduplicatedRecords) {
        for (Map.Entry<String, ConsumerRecord<String, String>> entry : deduplicatedRecords.entrySet()) {
            try {
                processSivaItemUpdateQueueEvent(entry.getValue());
            } catch (Exception processingException) {
                log.error(
                  "Error processing deduplicated record for itemSku: {}, partition: {}, offset: {}",
                  entry.getKey(), entry.getValue().partition(), entry.getValue().offset(),
                  processingException);
            }
        }
    }


    private boolean isEligibleForItemDataChangeEventProcessingV2(Item newItem) {
        return eventConsumptionProperties.isItemDataChangeV2EventEnabled() && !Optional.ofNullable(
                eventConsumptionProperties.getItemDataChangeV2EventBlackListedSellers()).orElseGet(ArrayList::new)
            .contains(newItem.getMerchantCode());
    }

    private boolean isEligibleForItemDataChangeEventProcessing(Item newItem) {
        return !Optional.ofNullable(eventConsumptionProperties.getItemDataChangeEventBlackListedSellers())
            .orElseGet(ArrayList::new).contains(newItem.getMerchantCode());
    }

    private void doOnItemEventV2(ConsumerRecord<String, String> record, String groupId, boolean migration,
        String traceId, Item newItem) {
        if (skipProductPublishL4Event && Boolean.FALSE.equals(newItem.isMarkForDelete())
            && SOURCE_PRODUCT_PUBLISH.equals(newItem.getSource())) {
            log.info("Skip L4 event processing for item : {} ", newItem.getItemSku());
            return;
        }
        if (Objects.isNull(newItem.getId())) {
            newItem.setId(newItem.getItemSku());
        }

        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(newItem.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.ITEM, false),
                traceId);
        LoggerUtil.sendSuccessListenLog(newItem, "onItemEvent", record.topic(), traceId);
        if (dataConverter.getDirectSaveFlagFromRawItemUpdateEventModel(record, newRawItemUpdateQueueEnabled)) {
            itemService.save(newItem, saveParam).block();
        } else {
            saveRawServiceV2.saveItem(newItem, saveParam);
        }
    }

    public void doOnItemEvent(Item newItem, String groupId, String topicSource, boolean migration, String command,
        String traceId) {
        if (skipProductPublishL4Event && Boolean.FALSE.equals(newItem.isMarkForDelete())
            && SOURCE_PRODUCT_PUBLISH.equals(newItem.getSource())) {
            log.info("Skip L4 event processing for item : {} ", newItem.getItemSku());
            return;
        }

        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(newItem.getTimestamp(), groupId, topicSource, migration,
            timeStampProperties.considerTimeStamp(Topics.ITEM, false), traceId);
        LoggerUtil.sendSuccessListenLog(newItem, command, topicSource, traceId);

        saveRawService.saveItem(newItem, saveParam, false).onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("Item"))
            .subscribe(s -> LoggerUtil.success(command, traceId), t -> LoggerUtil.error(t, command, traceId));
    }

    public void onPickupPointEventV2(ConsumerRecord<String, String> record, String groupId, boolean migration,
        boolean fullReconstruct, String traceId) {
        PickupPoint pickupPoint =
            dataConverter.toPickupPointFromCombinedUpsertEventModel(record, newItemPickupPointUpsertEventEnabled);
        try {
            Optional.ofNullable(pickupPoint)
                .ifPresent(newPickupPoint -> {
                    if (isEligibleForItemPickupPointEventProcessing(newPickupPoint)) {
                        if (isEligibleForItemPickupPointEventProcessingV2(newPickupPoint)) {
                            doOnPickupPointEventV2(newPickupPoint, groupId, record.topic(), migration, fullReconstruct,
                                "onPickupPointEvent", traceId);
                        } else {
                            doOnPickupPointEvent(newPickupPoint, groupId, record.topic(), migration, fullReconstruct,
                                "onPickupPointEvent", traceId);
                        }
                    }
                });
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming l5 event record : {} ", record, e);
        }
    }

    private boolean isEligibleForItemPickupPointEventProcessingV2(PickupPoint newPickupPoint) {
        String merchantCode = Optional.ofNullable(newPickupPoint)
            .map(PickupPoint::getMerchantCode)
            .orElse(null);
        return eventConsumptionProperties.isItemPickupPointDataChangeV2EventEnabled() && !Optional.ofNullable(
                eventConsumptionProperties.getItemPickupPointDataChangeV2EventBlackListedSellers())
            .orElseGet(ArrayList::new).contains(merchantCode);
    }

    private boolean isEligibleForItemPickupPointEventProcessing(PickupPoint newPickupPoint) {
        String merchantCode = Optional.ofNullable(newPickupPoint)
            .map(PickupPoint::getMerchantCode)
            .orElse(null);
        return !Optional.ofNullable(eventConsumptionProperties.getItemPickupPointDataChangeBlackListedSellers())
            .orElseGet(ArrayList::new).contains(merchantCode);
    }

    public void onPickupPointEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, boolean fullReconstruct, String traceId) {
        Optional.ofNullable(toData(record, PickupPoint.class))
            .ifPresent(newPickupPoint -> {
                doOnPickupPointEvent(newPickupPoint,groupId,record.topic(),migration,fullReconstruct,"onPickupPointEvent", traceId);
            });
    }

    public void doOnPickupPointEventV2(PickupPoint newPickupPoint, String groupId, String topicSource,
        boolean migration, boolean fullReconstruct, String command, String traceId) {
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(newPickupPoint.getTimestamp(), groupId, topicSource, migration,
                timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);
        Optional.ofNullable(saveParam).map(SaveParam::getDbParam)
            .ifPresent(val -> val.setFullReconstruct(fullReconstruct));
        LoggerUtil.sendSuccessListenLog(newPickupPoint, command, topicSource, traceId);
        saveRawServiceV2.savePickupPoint(newPickupPoint, saveParam);
    }

    public void doOnPickupPointEvent(PickupPoint newPickupPoint, String groupId, String topicSource, boolean migration, boolean fullReconstruct, String command, String traceId) {
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(newPickupPoint.getTimestamp(), groupId, topicSource, migration,
                timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);
        Optional.ofNullable(saveParam)
            .map(SaveParam::getDbParam)
            .ifPresent(val -> val.setFullReconstruct(fullReconstruct));
        LoggerUtil.sendSuccessListenLog(newPickupPoint,command,topicSource,traceId);

        saveRawService.savePickupPoint(newPickupPoint,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("PickupPoint"))
            .subscribe(
                s -> LoggerUtil.success(command,traceId),
                t -> LoggerUtil.error(t,command,traceId)
            );
    }

    public void onFlashsaleProductEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FlashsaleProduct event = toData(record,FlashsaleProduct.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.FLASHSALE_PRODUCT, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onFlashsaleProductEvent",record.topic(),traceId);

        saveRawService.saveFlashsaleProduct(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("FlashSaleProduct"))
            .subscribe(
                s -> LoggerUtil.success("onFlashsaleProductEvent",traceId),
                t -> LoggerUtil.error(t,"onFlashsaleProductEvent",traceId)
            );
    }

    public void onBusinessPartnerProfileChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        BusinessPartnerProfileChangeEvent event = toData(record,BusinessPartnerProfileChangeEvent.class);
        LoggerUtil.sendSuccessListenLog(event,"onBusinessPartnerProfileChangeEvent",record.topic(),traceId);

        tagService.selfRepublishFbbTag(event)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("BusinessPartnerProfileChange"))
            .subscribe(
                s -> LoggerUtil.success("onBusinessPartnerProfileChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onBusinessPartnerProfileChangeEvent",traceId)
            );
    }

    public void onFbbChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FbbChangeEvent event = toData(record,FbbChangeEvent.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.FBB_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onFbbChangeEvent",record.topic(),traceId);

        tagService.updateFBBTagInItem(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("FbbChange"))
            .subscribe(
                s -> LoggerUtil.success("onFbbChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onFbbChangeEvent",traceId)
            );
    }

    public void onTradeInChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        TradeInChangeEvent event = toData(record, TradeInChangeEvent.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.TRADE_IN_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onTradeInChangeEvent",record.topic(),traceId);

        tagService.updateTradeInTagInProduct(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("TradeInChange"))
            .subscribe(
                s -> LoggerUtil.success("onTradeInChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onTradeInChangeEvent",traceId)
            );
    }

    public void onDenpasarShippingLogisticOptionChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        DenpasarShippingLogisticOptionChange event = toData(record,DenpasarShippingLogisticOptionChange.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onDenpasarShippingLogisticOptionChangeEvent",record.topic(),traceId);

        tagService.updateShippingThdTagInItem(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DenpasarShippingLogisticOptionChange"))
            .subscribe(
                s -> LoggerUtil.success("onDenpasarShippingLogisticOptionChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onDenpasarShippingLogisticOptionChangeEvent",traceId)
            );
    }

    public void onDenpasarSearchLogisticOptionChangeEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        DenpasarSearchLogisticOptionChange event = toData(record,DenpasarSearchLogisticOptionChange.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onDenpasarSearchLogisticOptionChangeEvent",record.topic(),traceId);

        tagService.updateSearchThdTagInItem(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("ModuleProduct"))
            .subscribe(
                s -> LoggerUtil.success("onDenpasarSearchLogisticOptionChangeEvent",traceId),
                t -> LoggerUtil.error(t,"onDenpasarSearchLogisticOptionChangeEvent",traceId)
            );
    }

    public void onFBBItemEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        SivaItem event = toData(record,SivaItem.class);
        SivaItem l2SivaItem = sivaItemService.toL2SivaItem(event);
        SivaItem l4SivaItem = sivaItemService.toL4SivaItem(event);
        ModuleProductUtil.setMarkForDeleteIfL2AndL4Exists(l2SivaItem,l4SivaItem);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.FBB_ITEM_SAVE, false), traceId);

        Mono<Boolean> saveL2SivaItemCommand = mainUpdater.directUpdateSivaBothByFbbItem(l2SivaItem,saveParam);
        Mono<Boolean> saveL4SivaItemCommand = mainUpdater.directUpdateSivaBothByFbbItem(l4SivaItem,saveParam);

        Mono.zip(saveL2SivaItemCommand,saveL4SivaItemCommand, MainUtil::reduce)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("FbbItem"))
            .subscribe(
                s -> LoggerUtil.success("onFBBItemEvent",traceId),
                t -> LoggerUtil.error(t,"onFBBItemEvent",traceId)
            );
    }

    public void onDigitalFlashsaleEvent(ConsumerRecord<String, String> record, String groupId, boolean migration, String traceId) {
        SivaProduct event = toData(record,SivaProduct.class);
        SaveParam saveParam =
            ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), migration,
                timeStampProperties.considerTimeStamp(Topics.DIGITAL_FLASHSALE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onDigitalFlashsaleEvent",record.topic(),traceId);

        saveProcessedService.saveSivaProduct(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DigitalFlashSale"))
            .subscribe(
                s -> LoggerUtil.success("onDigitalFlashsaleEvent",traceId),
                t -> LoggerUtil.error(t,"onDigitalFlashsaleEvent",traceId)
            );
    }

    public void onCampaignProductPublishedEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignProductPublished event = toData(record,CampaignProductPublished.class);
        List<CampaignProduct> campaignProducts = campaignProductService.fromCampaignProductPublishedToCampaignProducts(event);
        List<CampaignProduct> filteredCampaignProducts =
            checkAndFilterBlackListCampaignProducts(campaignProducts);
        if(CollectionUtils.isEmpty(filteredCampaignProducts)) return;
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_PUBLISHED, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductPublishedEvent",record.topic(),traceId);

        saveRawService.saveCampaignProducts(filteredCampaignProducts,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProduct"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductPublishedEvent",traceId),
                t -> LoggerUtil.error(t,"onCampaignProductPublishedEvent",traceId)
            );
    }

    private List<CampaignProduct> checkAndFilterBlackListCampaignProducts(List<CampaignProduct> campaignProducts) {
        List<CampaignProduct> filteredCampaignProducts = campaignProducts.stream().filter(campaignProduct -> {
            String productSku = Optional.ofNullable(campaignProduct.getSku().getProductSku()).orElse(StringUtils.EMPTY);
            return eventConsumptionProperties.getCampaignProductPublishedEventBlackListedSellers().stream()
                .noneMatch(productSku::startsWith);
        }).collect(Collectors.toList());
        return filteredCampaignProducts;
    }

    public void onCampaignProductTagLabelEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignTeaserLive event = toData(record,CampaignTeaserLive.class);
        List<CampaignProduct> campaignProducts = campaignProductService.fromCampaignProductTagLabelToCampaignProducts(event);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductTagLabelEvent",record.topic(),traceId);

        saveRawService.saveCampaignProducts(campaignProducts,saveParam)
            .flatMap(r -> publisherService.publishCampaignProductTagLabel(event,saveParam,CollectionUtils.isNotEmpty(campaignProducts)))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProductTagLabelChange"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductTagLabelEvent",traceId),
                t -> LoggerUtil.error(t,"onCampaignProductTagLabelEvent",traceId)
            );
    }

    public void onCampaignProductEndedEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignProductEnded event = toData(record,CampaignProductEnded.class);
        List<CampaignProduct> campaignProducts = Optional.ofNullable(event)
            .map(ModuleProductUtil::toCampaignCodeSessionIds)
            .map(campaignProductService::getActiveCampaignProductsByCampaignSessions)
            .orElseGet(ArrayList::new)
            .stream()
            .peek(val -> val.setActive(false))
            .collect(Collectors.toList());
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_ENDED, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductEndedEvent",record.topic(),traceId);

        saveRawService.saveCampaignProducts(campaignProducts,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProductEnded"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductEndedEvent",traceId),
                t -> LoggerUtil.error(t,"onCampaignProductEndedEvent",traceId)
            );
    }

    public void onCampaignProductRemovedEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignProductRemoved event = toData(record,CampaignProductRemoved.class);
        List<CampaignProduct> campaignProducts = Optional.ofNullable(event)
            .map(ModuleProductUtil::toCampaignProductIds)
            .map(campaignProductService::getExistingCampaignProducts)
            .orElseGet(ArrayList::new)
            .stream()
            .peek(val -> val.setActive(false))
            .collect(Collectors.toList());
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_REMOVED, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductRemovedEvent",record.topic(),traceId);

        saveRawService.saveCampaignProducts(campaignProducts,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProductRemoved"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductRemovedEvent",traceId),
                t -> LoggerUtil.error(t,"onCampaignProductRemovedEvent",traceId)
            );
    }

    public void onCheapestPriceEvent(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CheapestPrice event = toData(record,CheapestPrice.class);
        List<CheapestPriceDay> cheapestPriceDays = cheapestPriceDayService.fromCheapestPriceToCheapestPriceDays(event);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CHEAPEST_PRICE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCheapestPriceEvent",record.topic(),traceId);

        saveRawService.saveCampaignProductDays(cheapestPriceDays,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CheapestPrice"))
            .subscribe(
                s -> LoggerUtil.success("onCheapestPriceEvent",traceId),
                t -> LoggerUtil.error(t,"onCheapestPriceEvent",traceId)
            );
    }

    public void onCampaignProductLiveEventSivaCampaignProduct(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignProductLive event = toData(record,CampaignProductLive.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_LIVE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductLiveEventSivaCampaignProduct",record.topic(),traceId);

        mainUpdater.directUpdateSivaCampaignProductByCampaignProductLive(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProductLive"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductLiveEventSivaCampaignProduct", traceId),
                t -> LoggerUtil.error(t,"onCampaignProductLiveEventSivaCampaignProduct", traceId)
            );
    }

    public void onCampaignProductEndedEventSivaCampaignProduct(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignProductEnded event = toData(record,CampaignProductEnded.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_PRODUCT_ENDED, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductEndedEventSivaCampaignProduct",record.topic(),traceId);

        mainUpdater.directUpdateSivaCampaignProductByCampaignProductEnded(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignProductEnded"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductEndedEventSivaCampaignProduct",traceId),
                t -> LoggerUtil.error(t,"onCampaignProductEndedEventSivaCampaignProduct",traceId)
            );
    }

    public void onCampaignTeaserLiveEventSivaCampaignProduct(ConsumerRecord<String, String> record, String groupId, String traceId) {
        CampaignTeaserLive event = toData(record,CampaignTeaserLive.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CAMPAIGN_TEASER_LIVE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignTeaserLiveEventSivaCampaignProduct",record.topic(),traceId);

        Mono<Boolean> campaignTeaserLiveMono;
        if (publisherProperties.isThrottlingEnabled()) {
            campaignTeaserLiveMono = Flux.just(event).flatMap(
              e -> mainUpdater.directUpdateSivaCampaignProductByCampaignTeaserLive(e, saveParam)
                .flatMap(r -> publisherService.publishCampaignProductTagLabel(e, saveParam, true)),
              publisherProperties.getConcurrency()).reduce(MainUtil::reduce);
        } else {
            campaignTeaserLiveMono = Flux.just(event).flatMap(
                e -> mainUpdater.directUpdateSivaCampaignProductByCampaignTeaserLive(e, saveParam)
                  .flatMap(r -> publisherService.publishCampaignProductTagLabel(e, saveParam, true)))
              .reduce(MainUtil::reduce);
        }

        campaignTeaserLiveMono
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CampaignTeaserLive"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignTeaserLiveEventSivaCampaignProduct", traceId),
                t -> LoggerUtil.error(t,"onCampaignTeaserLiveEventSivaCampaignProduct", traceId)
            );
    }

    public void onCampaignProductDeleteAllExpiredEventSivaCampaignProduct(ConsumerRecord<String, String> record, String groupId, String traceId) {
        DeleteExpiredEvent event = toData(record,DeleteExpiredEvent.class);
        LoggerUtil.sendSuccessListenLog(event,"onCampaignProductDeleteAllExpiredEventSivaCampaignProduct", record.topic(), traceId);

        mainService.deleteExpired(event)
            .flatMap(r -> schedulerService.scheduleDeleteSivaCampaignProduct(event,traceId))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DeleteExpired"))
            .subscribe(
                s -> LoggerUtil.success("onCampaignProductDeleteAllExpiredEventSivaCampaignProduct", traceId),
                t -> LoggerUtil.error(t,"onCampaignProductDeleteAllExpiredEventSivaCampaignProduct", traceId)
            );
    }

    public void onFlashsaleScheduleEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FlashsaleSchedule event = toData(record,FlashsaleSchedule.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.FLASHSALE_SCHEDULE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onFlashsaleScheduleEventSivaFlashsaleSchedule",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleScheduleByFlashsaleSchedule(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("FlashSaleSchedule"))
            .subscribe(
                s -> LoggerUtil.success("onFlashsaleScheduleEventSivaFlashsaleSchedule",traceId),
                t -> LoggerUtil.error(t,"onFlashsaleScheduleEventSivaFlashsaleSchedule",traceId)
            );
    }

    public void onCleanEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record, String groupId, String traceId) {
        IdTimestamp event = toData(record,IdTimestamp.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CLEAN_FLASHSALE_SCHEDULE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCleanEventSivaFlashsaleSchedule",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleScheduleClean(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("ClearSivaFlashSaleSchedule"))
            .subscribe(
                s -> LoggerUtil.success("onCleanEventSivaFlashsaleSchedule",traceId),
                t -> LoggerUtil.error(t,"onCleanEventSivaFlashsaleSchedule",traceId)
            );
    }

    public void onDeactivateEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FlashsaleProduct event = toData(record,FlashsaleProduct.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.DEACTIVATE_FLASHSALE_SCHEDULE, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onDeactivateEventSivaFlashsaleSchedule",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleScheduleDeactivate(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DeactivateSivaFlashSaleSchedule"))
            .subscribe(
                s -> LoggerUtil.success("onDeactivateEventSivaFlashsaleSchedule", traceId),
                t -> LoggerUtil.error(t,"onDeactivateEventSivaFlashsaleSchedule", traceId)
            );
    }

    public void onDeleteAllExpiredEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record, String groupId, String traceId) {
        DeleteExpiredEvent event = toData(record,DeleteExpiredEvent.class);
        LoggerUtil.sendSuccessListenLog(event,"onDeleteAllExpiredEventSivaFlashsaleSchedule",record.topic(),traceId);

        mainService.deleteExpired(event)
            .flatMap(r -> schedulerService.scheduleDeleteSivaFlashsaleSchedule(event,traceId))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DeleteSivaFlashSaleSchedule"))
            .subscribe(
                s -> LoggerUtil.success("onDeleteAllExpiredEventSivaFlashsaleSchedule", traceId),
                t -> LoggerUtil.error(t,"onDeleteAllExpiredEventSivaFlashsaleSchedule", traceId)
            );
    }

    public void onFlashsaleGroupEventSivaFlashsaleGroup(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FlashsaleGroup event = toData(record,FlashsaleGroup.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.FLASHSALE_GROUP, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onFlashsaleGroupEventSivaFlashsaleGroup",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleGroupByFlashsaleGroup(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("GroupSivaFlashSale"))
            .subscribe(
                s -> LoggerUtil.success("onFlashsaleGroupEventSivaFlashsaleGroup", traceId),
                t -> LoggerUtil.error(t,"onFlashsaleGroupEventSivaFlashsaleGroup", traceId)
            );
    }

    public void onCleanEventSivaFlashsaleGroup(ConsumerRecord<String, String> record, String groupId, String traceId) {
        IdTimestamp event = toData(record,IdTimestamp.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.CLEAN_FLASHSALE_GROUP, false), traceId);
        LoggerUtil.sendSuccessListenLog(event,"onCleanEventSivaFlashsaleGroup",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleGroupClean(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("CleanGroupSivaFlashSale"))
            .subscribe(
                s -> LoggerUtil.success("onCleanEventSivaFlashsaleGroup", traceId),
                t -> LoggerUtil.error(t,"onCleanEventSivaFlashsaleGroup", traceId)
            );
    }

    public void onDeactivateEventSivaFlashsaleGroup(ConsumerRecord<String, String> record, String groupId, String traceId) {
        FlashsaleProduct event = toData(record,FlashsaleProduct.class);
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(event.getTimestamp(), groupId, record.topic(), false,
            timeStampProperties.considerTimeStamp(Topics.DEACTIVATE_FLASHSALE_GROUP, false),traceId);
        LoggerUtil.sendSuccessListenLog(event,"onDeactivateEventSivaFlashsaleGroup",record.topic(),traceId);

        mainUpdater.directUpdateSivaFlashsaleGroupDeactivate(event,saveParam)
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DeactivateGroupSivaFlashSale"))
            .subscribe(
                s -> LoggerUtil.success("onDeactivateEventSivaFlashsaleGroup", traceId),
                t -> LoggerUtil.error(t,"onDeactivateEventSivaFlashsaleGroup", traceId)
            );
    }

    public void onDeleteAllExpiredEventSivaFlashsaleGroup(ConsumerRecord<String, String> record, String groupId, String traceId) {
        DeleteExpiredEvent event = toData(record,DeleteExpiredEvent.class);
        LoggerUtil.sendSuccessListenLog(event,"onDeleteAllExpiredEventSivaFlashsaleGroup",record.topic(),traceId);

        mainService.deleteExpired(event)
            .flatMap(r -> schedulerService.scheduleDeleteSivaFlashsaleGroup(event,traceId))
            .onErrorResume(MainUtil::errorResult)
            .subscribeOn(schedulerHelper.of("DeleteGroupSivaFlashSale"))
            .subscribe(
                s -> LoggerUtil.success("onDeleteAllExpiredEventSivaFlashsaleGroup", traceId),
                t -> LoggerUtil.error(t,"onDeleteAllExpiredEventSivaFlashsaleGroup", traceId)
            );
    }

    public void onSivaProductCombinedUpsertEvent(ConsumerRecord<String, String> record, String traceId) {
        SivaProductCombinedUpsertEventModel event = toData(record,SivaProductCombinedUpsertEventModel.class);
        log.info("Performing Siva product upsert for {} ", event.getEventTrigger());
        SivaProduct sivaProduct = event.getSivaProduct();
        SaveParam saveParam = event.getSaveParam();
        LoggerUtil.sendSuccessListenLog(event,"onSivaProductCombinedUpsertEvent", record.topic(),traceId);
        saveProcessedService.saveSivaProduct(sivaProduct,saveParam).onErrorResume(MainUtil::errorResult).subscribe(
          s -> LoggerUtil.success("onSivaProductCombinedUpsertEvent", traceId),
          t -> LoggerUtil.error(t,"onSivaProductCombinedUpsertEvent", traceId)
        );
    }

    public void onPickupPointNewEvent(ConsumerRecord<String, String> record, boolean migration, boolean fullReconstruct,
        String traceId) {
        try {
            Optional.ofNullable(getObjectMapper().readValue(record.value(), PickupPoint.class))
                .ifPresent(pickupPoint -> {
                    if (ModuleProductUtil.isProductRejected(
                      pickupPoint.getItemPickupPointChangeEventTypesV2(), traceId,
                      pickupPoint.getId(), Collections.PICKUP_POINT, deleteProductDataOnTermination,
                      getChangeTypesToSkipL4AndL5Events())) {
                        return;
                    }
                    PickupPointUpsertCombinedEventModel eventModel =
                      dataConverter.getPickupPointUpsertCombinedEventModel(record, migration,
                        fullReconstruct, pickupPoint, null, null, null, traceId);
                    log.info("Created PickupPointUpsertCombinedEventModel: {}", eventModel);
                    publisherService.publishSivaPickupPointUpsertCombinedEvent(eventModel).thenReturn(true)
                        .onErrorReturn(false).block();
                });
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming l5 event record : {} ", record, e);
        }
    }

    private List<String> getChangeTypesToSkipL4AndL5Events() {
        if (changeTypesToSkipItemAndPickupPointEvent == null || changeTypesToSkipItemAndPickupPointEvent.isEmpty()) {
            return java.util.Collections.emptyList();
        }
        return List.of(changeTypesToSkipItemAndPickupPointEvent.split(COMMA));
    }

  public void onPickupPointCombinedUpsertEvent(ConsumerRecord<String, String> record) {
    try {
      PickupPointUpsertCombinedEventModel eventModel =
        getObjectMapper().readValue(record.value(), PickupPointUpsertCombinedEventModel.class);

      PickupPointEventType eventType = PickupPointEventType.from(eventModel);
      switch (eventType) {
        case PICKUP_POINT -> onPickupPointEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT,
          eventModel.isMigration(), eventModel.isFullReconstruct(), eventModel.getTraceId());
        case INVENTORY_INFO_CHANGE ->
          onInventoryInfoChangeEvent(record, Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE,
            eventModel.getTraceId());
        case STOCK_UPDATE_SEARCH -> processInventoryStockUpdate(eventModel.getStockUpdateSearchEvent(),
          eventModel.getTraceId(), record.topic());
        case INVENTORY_REPUBLISH ->
          processInventoryOnStockRepublish(eventModel.getLevel2InventoryQuantityChangedEvent(),
            eventModel.getTraceId(), record.topic());
        default -> log.info("No valid object found to process in event model: {}", record);
      }
    } catch (Exception e) {
      TraceHelper.recordError(tracer, e);
      log.error("Error while consuming L5 upsert event record: {}", record, e);
    }
  }


    private void processInventoryStockUpdate(StockUpdateSearchEvent stockUpdateSearchEvent, String traceId,
      String topic) {
        log.info("[DEBUG] Processing stock update event - Event: {}, TraceId: {}, Topic: {}", 
                stockUpdateSearchEvent, traceId, topic);
        
        Optional.ofNullable(stockUpdateSearchEvent)
          .map(val -> {
              String pickupPointId = ModuleProductUtil.toPickupPointId(val.getWebItemSku(),val.getPickupPointCode());
              log.info("[DEBUG] Generated pickup point ID: {}", pickupPointId);
              return pickupPointId;
          })
          .ifPresent(pickupPointId -> {
              PickupPoint pickupPoint = pickupPointService.getExistingPickupPoint(pickupPointId);
              if (Objects.nonNull(pickupPoint)) {
                  pickupPoint.setTimestamp(stockUpdateSearchEvent.getTimestamp());
                  onStockUpdateEvent(pickupPoint, stockUpdateSearchEvent, topic, traceId);
              } else {
                  // Missing pickup point: create inventory entry directly
                  createInventoryWithoutPickupPoint(stockUpdateSearchEvent, traceId, topic);
              }
          });
    }

    private void createInventoryWithoutPickupPoint(StockUpdateSearchEvent stockUpdateSearchEvent, String traceId, String topic) {
      PickupPointInventory inventory =
        getPickupPointInventory(stockUpdateSearchEvent);
      SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(stockUpdateSearchEvent.getTimestamp(),
        Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE, topic, false,
        timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);

      try {
        pickupPointInventoryService.save(inventory, saveParam).doOnSuccess(success -> {
        }).doOnError(
          error -> log.error("[DEBUG] Failed to save standalone PickupPointInventory with ID: {}",
            inventory.getId(), error)).block();
      } catch (Exception e) {
        log.error(
          "[DEBUG] Failed to save standalone PickupPointInventory with ID: {}, eventTimestamp: {}",
          inventory.getId(), inventory.getEventTimestamp(), e);
      }
    }

  private static PickupPointInventory getPickupPointInventory(
    StockUpdateSearchEvent stockUpdateSearchEvent) {
    PickupPointInventory inventory =
      PickupPointInventory.builder().pickupPointCode(stockUpdateSearchEvent.getPickupPointCode())
        .itemSku(stockUpdateSearchEvent.getWebItemSku())
        .inStock(!stockUpdateSearchEvent.isOutOfStock())
        .syncStock(stockUpdateSearchEvent.isSyncStock())
        .updatedTimestamp(stockUpdateSearchEvent.getTimestamp())
        .createdTimestamp(stockUpdateSearchEvent.getTimestamp())
        .timestamp(stockUpdateSearchEvent.getTimestamp())
        .eventTimestamp(stockUpdateSearchEvent.getTimestamp()).build();
    inventory.setId(inventory.toId());
    return inventory;
  }

  private void onStockUpdateEvent(PickupPoint pickupPoint,
      StockUpdateSearchEvent stockUpdateSearchEvent, String topic, String traceId) {
        SaveParam saveParam =
          ParamUtil.toSaveParamDirectUpdate(pickupPoint.getTimestamp(),
            Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE, topic, false,
            timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);
        Optional.ofNullable(saveParam).map(SaveParam::getDbParam)
          .ifPresent(val -> val.setFullReconstruct(false));
        LoggerUtil.sendSuccessListenLog(pickupPoint, STOCK_UPDATE_EVENT_COMMAND, topic, traceId);
        saveRawServiceV2.savePickupPointOnInventoryUpdate(pickupPoint, stockUpdateSearchEvent,
          saveParam);

    }


    public void onItemUpdateNewEvent(ConsumerRecord<String, String> record, boolean migration,
      String traceId) {
        try {
            Optional.ofNullable(getObjectMapper().readValue(record.value(), Item.class))
              .ifPresent(item -> {
                  if (ModuleProductUtil.isProductRejected(item.getItemChangeEventTypesV2(), traceId,
                    item.getId(), Collections.ITEM, deleteProductDataOnTermination,
                    getChangeTypesToSkipL4AndL5Events())) {
                      return;
                  }
                RawItemCombinedUpsertEventModel rawItemCombinedUpsertEventModel =
                    ModuleProductUtil.toRawItemCombinedUpsertEventModel(item, migration, false);
                log.info("publishing : {} ", rawItemCombinedUpsertEventModel);
                rawItemCombinedUpsertEventModel.setId(item.getProductSku());
                publisherService.publishNewRawItemUpsertCombinedEvent(rawItemCombinedUpsertEventModel).thenReturn(true)
                    .onErrorReturn(false).block();
            });
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming l4 event record : {} ", record, e);
        }
    }

    public void onInventoryInfoChangeNewEvent(ConsumerRecord<String, String> record, String traceId) {
        try {
            Optional.ofNullable(getObjectMapper().readValue(record.value(), InventoryInfoChange.class))
                .ifPresent(inventoryInfoChange -> {
                    PickupPointUpsertCombinedEventModel eventModel =
                      dataConverter.getPickupPointUpsertCombinedEventModel(record, false, false,
                        null, inventoryInfoChange, null, null, traceId);
                    log.info("Created PickupPointUpsertCombinedEventModel: {}", eventModel);
                    publisherService.publishSivaPickupPointUpsertCombinedEvent(eventModel).thenReturn(true)
                        .onErrorReturn(false).block();
                });
        } catch (Exception e) {
            TraceHelper.recordError(tracer,e);
            log.error("Error while consuming inventory info  event record : {} ", record, e);
        }
    }

    public void saveProductAndRelated(ConsumerRecord<String, String> record, String traceId)
      throws JsonProcessingException {
        RawProductCombinedUpsertEventModel rawProductCombinedUpsertEventModel =
          getObjectMapper().readValue(record.value(), RawProductCombinedUpsertEventModel.class);
        log.info("Listened to event to perform Raw Product save for product : {} ",
          rawProductCombinedUpsertEventModel.getId());
        Product product = rawProductCombinedUpsertEventModel.getProduct();
        SaveParam saveParam = rawProductCombinedUpsertEventModel.getSaveParam();
        saveRawService.saveProduct(product, saveParam, false).onErrorResume(MainUtil::errorResult)
          .subscribeOn(schedulerHelper.of("Product"))
          .doOnSuccess(s -> LoggerUtil.success("Product event processed successfully", traceId))
          .doOnError(t -> LoggerUtil.error(t, "Error processing product event", traceId))
          .subscribe();
    }

  public void onPermanentDeleteDataEvent(List<ConsumerRecord<String, String>> records,
    String traceId) {
    List<TerminatedSellerDeletionEventModel> validEvents = new ArrayList<>();
    for (ConsumerRecord<String, String> record : records) {
      try {
        TerminatedSellerDeletionEventModel model =
          getObjectMapper().readValue(record.value(), TerminatedSellerDeletionEventModel.class);
        if (ModuleProductUtil.validateEventRecordForDeletion(model)) {
          validEvents.add(model);
        } else {
          log.warn("Invalid event for product SKU={} traceId={}", model.getProductSku(), traceId);
        }
      } catch (Exception e) {
        log.error("Failed to parse record: key={} traceId={}", record.key(), traceId, e);
      }
    }
    if (!validEvents.isEmpty()) {
      log.info("Performing bulk delete for {} products, traceId={}", validEvents.size(), traceId);
      productDeletionService.performDeletionFromAllDataSources(validEvents, traceId)
        .doOnError(e -> log.error("Error during bulk deletion, traceId={}", traceId, e))
        .subscribe();
    }
  }

  public void onElasticSearchDeletionEvent(ConsumerRecord<String, String> record, String traceId) {
    try {
      ProductElasticSearchDeletionEvent deletionEvent =
        getObjectMapper().readValue(record.value(), ProductElasticSearchDeletionEvent.class);

      if (Objects.isNull(deletionEvent)) {
        log.error("Failed to deserialize ProductElasticSearchDeletionEvent from record, traceId={}", traceId);
        return;
      }

      Set<String> productSkus = deletionEvent.getProductSkus();
      if (CollectionUtils.isEmpty(productSkus)) {
        log.error("No productSkus found in deletion event, traceId={}", traceId);
        return;
      }

      SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(deletionEvent.getTimestamp(),
        Topics.PRODUCT_ES_DELETION_EVENT, record.topic(), false,
        timeStampProperties.considerTimeStamp(Topics.PRODUCT_ES_DELETION_EVENT, false), traceId);

      log.debug("Starting synchronous ES deletion for {} productSku(s), traceId={} ",
        productSkus.size(), traceId);

      for (String productSku : productSkus) {
        try {
          if (Objects.nonNull(productSku)) {
            productDeletionService.performElasticSearchDeletion(productSku, saveParam);
            log.info("Successfully deleted productSku={} from ES, traceId={} ", productSku, traceId);
          } else {
            log.warn("Skipping null or empty productSku in deletion event, traceId={} ", traceId);
          }
        } catch (Exception e) {
          log.error("Failed ES deletion for productSku={}, traceId={} ", productSku, traceId, e);
        }
      }
    } catch (Exception e) {
      log.error("Unexpected error in onElasticSearchDeletionEvent, traceId={}", traceId, e);
    }
  }


    public void publishPickupPointUpsertEventOnInventoryStockUpdate(ConsumerRecord<String, String> record, String traceId) {
        try {
          StockUpdateSearchEvent stockUpdateSearchEvent =
            getObjectMapper().readValue(record.value(), StockUpdateSearchEvent.class);
          if (Objects.nonNull(stockUpdateSearchEvent)) {
            PickupPointUpsertCombinedEventModel eventModel =
              getPickupPointUpsertCombinedEventModel(record, traceId, stockUpdateSearchEvent);
            if (stockUpdateSearchEvent.isDataUpdated()) {
              publisherService.publishSivaPickupPointUpsertCombinedEvent(eventModel)
                .doOnError(e -> {
                  TraceHelper.recordError(tracer, e);
                  log.error("Error while publishing event [traceId={}]: {}", traceId, eventModel,
                    e);
                }).block();
            } else {
              log.error("Skipped Pickup Point event due to stale event consumption for {} ",
                ModuleProductUtil.toPickupPointId(stockUpdateSearchEvent.getWebItemSku(),
                  stockUpdateSearchEvent.getPickupPointCode()));
            }
          }

        } catch (Exception e) {
            TraceHelper.recordError(tracer, e);
            log.error("Error while consuming inventory Stock Update, record : {} ", record, e);
        }
    }

  private SaveParam getSaveParamForInventoryUpdate(String traceId,
    PickupPointUpsertCombinedEventModel eventModel, String topicName) {
    Long timestamp = null;
    if (Objects.nonNull(eventModel.getPickupPoint())) {
      timestamp = eventModel.getPickupPoint().getTimestamp();
    } else if (Objects.nonNull(eventModel.getLevel2InventoryQuantityChangedEvent())) {
      timestamp = eventModel.getLevel2InventoryQuantityChangedEvent().getTimestamp();
    } else if (Objects.nonNull(eventModel.getStockUpdateSearchEvent())) {
      timestamp = eventModel.getStockUpdateSearchEvent().getTimestamp();
    }
    return ParamUtil.toSaveParamDirectUpdate(timestamp,
      Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE,
      topicName, false,
      timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);
  }

  public void publishPickupPointUpsertEventOnInventoryRepublish(ConsumerRecord<String, String> record, String traceId) {
        try {
            Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent =
              getObjectMapper().readValue(record.value(), Level2InventoryQuantityChangedEvent.class);
            if (Objects.nonNull(level2InventoryQuantityChangedEvent)) {
              PickupPointUpsertCombinedEventModel eventModel =
                getPickupPointUpsertCombinedEventModel(record, traceId,
                  level2InventoryQuantityChangedEvent);
              if (level2InventoryQuantityChangedEvent.isDataUpdated()) {
                publisherService.publishSivaPickupPointUpsertCombinedEvent(eventModel)
                  .doOnError(e -> {
                    TraceHelper.recordError(tracer, e);
                    log.error("Error while publishing event [traceId={}]: {}", traceId, eventModel,
                      e);
                  }).block();
              } else {
                log.error("Skipped Pickup Point event due to stale event consumption for {} ",
                  ModuleProductUtil.toPickupPointId(
                    level2InventoryQuantityChangedEvent.getWarehouseItemSku(),
                    level2InventoryQuantityChangedEvent.getPickupPointCode()));
              }
            }

        } catch (Exception e) {
            TraceHelper.recordError(tracer, e);
            log.error("Error while consuming inventory Stock Update, record : {} ", record, e);
        }
    }

  private PickupPointUpsertCombinedEventModel getPickupPointUpsertCombinedEventModel(
    ConsumerRecord<String, String> record, String traceId,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    PickupPointUpsertCombinedEventModel eventModel =
      dataConverter.getPickupPointUpsertCombinedEventModel(record, false, false, null, null, null,
        level2InventoryQuantityChangedEvent, traceId);
    saveRawServiceV2.updateInventoryDataOnRepublishEvent(level2InventoryQuantityChangedEvent,
      getSaveParamForInventoryUpdate(traceId, eventModel, Topics.INVENTORY_ONLINE_REPUBLISH_EVENT));
    return eventModel;
  }

  private PickupPointUpsertCombinedEventModel getPickupPointUpsertCombinedEventModel(
    ConsumerRecord<String, String> record, String traceId,
    StockUpdateSearchEvent stockUpdateSearchEvent) {
    PickupPointUpsertCombinedEventModel eventModel =
      dataConverter.getPickupPointUpsertCombinedEventModel(record, false, false, null, null,
        stockUpdateSearchEvent, null, traceId);
    saveRawServiceV2.performInventoryDataSaveOnStockUpdateEvent(
      eventModel.getStockUpdateSearchEvent(),
      getSaveParamForInventoryUpdate(traceId, eventModel, Topics.INVENTORY_STOCK_UPDATE_EVENT));
    return eventModel;
  }

  private void processInventoryOnStockRepublish(
      Level2InventoryQuantityChangedEvent stockRepublishEvent, String traceId, String topic) {
        Optional.ofNullable(stockRepublishEvent).map(
            val -> ModuleProductUtil.toPickupPointId(val.getLevel2Id(), val.getPickupPointCode()))
          .map(pickupPointService::getExistingPickupPoint).map(pickupPoint -> {
              pickupPoint.setTimestamp(stockRepublishEvent.getTimestamp());
              return pickupPoint;
          }).ifPresent(
            pickupPoint -> onStockRepublishEvent(pickupPoint, stockRepublishEvent, topic,
              traceId));
    }

    private void onStockRepublishEvent(PickupPoint pickupPoint,
      Level2InventoryQuantityChangedEvent stockRepublishEvent, String topic, String traceId) {
        SaveParam saveParam = ParamUtil.toSaveParamDirectUpdate(pickupPoint.getTimestamp(),
          Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE, topic, false,
          timeStampProperties.considerTimeStamp(Topics.PICKUP_POINT, false), traceId);
        Optional.ofNullable(saveParam).map(SaveParam::getDbParam)
          .ifPresent(val -> val.setFullReconstruct(false));
        LoggerUtil.sendSuccessListenLog(pickupPoint, STOCK_REPUBLISH_EVENT_COMMAND, topic, traceId);
        saveRawServiceV2.savePickupPointOnInventoryStockRepublish(pickupPoint, stockRepublishEvent,
          saveParam);

    }
}

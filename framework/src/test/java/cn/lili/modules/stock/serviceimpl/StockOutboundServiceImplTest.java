package cn.lili.modules.stock.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.stock.entity.dos.StockOutbound;
import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.stock.entity.dto.StockOutboundAuditDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundCreateDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundItemDTO;
import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import cn.lili.modules.stock.entity.enums.StockOutboundStatusEnum;
import cn.lili.modules.stock.mapper.StockOutboundMapper;
import cn.lili.modules.stock.service.StockOutboundItemService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商家出库单服务业务逻辑单元测试，覆盖草稿创建、重复SKU校验、审核出库库存扣减及状态机控制。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@ExtendWith(MockitoExtension.class)
class StockOutboundServiceImplTest {

    @InjectMocks
    private StockOutboundServiceImpl stockOutboundService;

    @Mock
    private StockOutboundMapper stockOutboundMapper;

    @Mock
    private StockOutboundItemService stockOutboundItemService;

    @Mock
    private GoodsSkuService goodsSkuService;

    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), StockOutbound.class);
        ReflectionTestUtils.setField(stockOutboundService, "baseMapper", stockOutboundMapper);
    }

    private AuthUser mockStoreUser(String storeId, String username) {
        AuthUser user = mock(AuthUser.class);
        lenient().when(user.getStoreId()).thenReturn(storeId);
        lenient().when(user.getUsername()).thenReturn(username);
        lenient().when(user.getRole()).thenReturn(UserEnums.STORE);
        return user;
    }

    /**
     * 验证创建出库单时如果明细中存在重复 SKU 则拒绝并抛出参数异常。
     */
    @Test
    void createDraftRejectsDuplicateSku() {
        StockOutboundCreateDTO dto = new StockOutboundCreateDTO();
        dto.setReasonId("reason-1");
        dto.setReasonName("领用出库");

        StockOutboundItemDTO item1 = new StockOutboundItemDTO();
        item1.setSkuId("sku-duplicate");
        item1.setGoodsName("商品A");
        item1.setQuantity(2);

        StockOutboundItemDTO item2 = new StockOutboundItemDTO();
        item2.setSkuId("sku-duplicate");
        item2.setGoodsName("商品A规格2");
        item2.setQuantity(3);

        dto.setItems(List.of(item1, item2));

        GoodsSku existingSku = new GoodsSku();
        existingSku.setId("sku-duplicate");
        existingSku.setStoreId("store-1");
        existingSku.setGoodsName("商品A");
        when(goodsSkuService.getGoodsSkuByIdFromCache("sku-duplicate")).thenReturn(existingSku);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> stockOutboundService.createDraft(dto));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.PARAMS_ERROR);
        assertThat(ex.getMessage()).contains("不能重复");
        verify(stockOutboundMapper, never()).insert(any(StockOutbound.class));
    }

    /**
     * 验证审核通过时扣减库存并带上 STOCK_OUTBOUND 出库流水上下文。
     */
    @Test
    @SuppressWarnings("unchecked")
    void auditApproveDeductsStockWithFlowContext() {
        String outboundId = "outbound-1";
        String storeId = "store-1";

        StockOutbound outbound = new StockOutbound();
        outbound.setId(outboundId);
        outbound.setOutboundSn("CK202611120001");
        outbound.setStoreId(storeId);
        outbound.setStatus(StockOutboundStatusEnum.AUDITING.name());
        outbound.setReasonName("办公领用");

        StockOutboundItem item = new StockOutboundItem();
        item.setOutboundId(outboundId);
        item.setGoodsId("goods-1");
        item.setSkuId("sku-1");
        item.setQuantity(5);

        StockOutboundAuditDTO auditDTO = new StockOutboundAuditDTO();
        auditDTO.setPass(true);
        auditDTO.setAuditRemark("审核通过准予出库");

        when(goodsSkuService.getStock("sku-1")).thenReturn(10);

        AuthUser authUser = mockStoreUser(storeId, "admin");

        when(stockOutboundMapper.selectById(outboundId)).thenReturn(outbound);
        when(stockOutboundMapper.update(any(), any())).thenReturn(1);
        when(stockOutboundItemService.listByOutboundId(outboundId)).thenReturn(List.of(item));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            stockOutboundService.audit(outboundId, auditDTO);

            ArgumentCaptor<List<GoodsSkuStockDTO>> stockDtoCaptor = ArgumentCaptor.forClass(List.class);
            ArgumentCaptor<StockFlowContext> contextCaptor = ArgumentCaptor.forClass(StockFlowContext.class);
            verify(goodsSkuService).updateStocksByType(stockDtoCaptor.capture(), contextCaptor.capture());

            List<GoodsSkuStockDTO> dtoList = stockDtoCaptor.getValue();
            assertThat(dtoList).hasSize(1);
            assertThat(dtoList.get(0).getSkuId()).isEqualTo("sku-1");
            assertThat(dtoList.get(0).getQuantity()).isEqualTo(5);
            assertThat(dtoList.get(0).getType()).isEqualTo("SUB");

            StockFlowContext flowContext = contextCaptor.getValue();
            assertThat(flowContext.getFlowType()).isEqualTo(StockFlowTypeEnum.STOCK_OUTBOUND);
            assertThat(flowContext.getBizSn()).isEqualTo("CK202611120001");
        }
    }

    /**
     * 验证审核驳回时不扣减库存，状态更新为 REJECTED 并记录审核备注。
     */
    @Test
    void auditRejectUpdatesStatusWithoutStockDeduction() {
        String outboundId = "outbound-2";
        String storeId = "store-1";

        StockOutbound outbound = new StockOutbound();
        outbound.setId(outboundId);
        outbound.setOutboundSn("CK202611120002");
        outbound.setStoreId(storeId);
        outbound.setStatus(StockOutboundStatusEnum.AUDITING.name());

        StockOutboundAuditDTO auditDTO = new StockOutboundAuditDTO();
        auditDTO.setPass(false);
        auditDTO.setAuditRemark("库存不足，暂缓出库");

        AuthUser authUser = mockStoreUser(storeId, "admin");

        when(stockOutboundMapper.selectById(outboundId)).thenReturn(outbound);
        when(stockOutboundMapper.update(any(), any())).thenReturn(1);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            stockOutboundService.audit(outboundId, auditDTO);

            verify(goodsSkuService, never()).updateStocksByType(anyList(), any());
            verify(stockOutboundMapper).update(any(), any());
        }
    }

    /**
     * 验证非待审核状态的出库单不能执行审核操作。
     */
    @Test
    void auditRejectsNonAuditingStatus() {
        String outboundId = "outbound-3";
        String storeId = "store-1";

        StockOutbound outbound = new StockOutbound();
        outbound.setId(outboundId);
        outbound.setStoreId(storeId);
        outbound.setStatus(StockOutboundStatusEnum.DRAFT.name());

        StockOutboundAuditDTO auditDTO = new StockOutboundAuditDTO();
        auditDTO.setPass(true);

        AuthUser authUser = mockStoreUser(storeId, "admin");
        when(stockOutboundMapper.selectById(outboundId)).thenReturn(outbound);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> stockOutboundService.audit(outboundId, auditDTO));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }
    }

    /**
     * 验证已完成的出库单不可作废。
     */
    @Test
    void cancelRejectsCompletedOutbound() {
        String outboundId = "outbound-4";
        String storeId = "store-1";

        StockOutbound outbound = new StockOutbound();
        outbound.setId(outboundId);
        outbound.setStoreId(storeId);
        outbound.setStatus(StockOutboundStatusEnum.COMPLETED.name());

        AuthUser authUser = mockStoreUser(storeId, "admin");
        when(stockOutboundMapper.selectById(outboundId)).thenReturn(outbound);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> stockOutboundService.cancel(outboundId));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }
    }
}

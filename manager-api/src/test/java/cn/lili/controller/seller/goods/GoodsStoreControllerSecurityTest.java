package cn.lili.controller.seller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.service.GoodsService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商家端商品控制器越权防护单元测试，验证跨店铺编辑商品时通过 OperationalJudgment 阻断。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class GoodsStoreControllerSecurityTest {

    private static final String CURRENT_STORE_ID = "store-current";
    private static final String OTHER_STORE_ID = "store-other";
    private static final String GOODS_ID = "goods-100";

    @InjectMocks
    private GoodsStoreController controller;

    @Mock
    private GoodsService goodsService;

    private AuthUser currentStoreUser() {
        return AuthUser.builder()
                .id("user-1")
                .storeId(CURRENT_STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    @Test
    @DisplayName("商家修改其他店铺商品时在Controller层被拒绝")
    void updateOtherStoreGoodsThrowsAuthorityError() {
        Goods otherGoods = new Goods();
        otherGoods.setId(GOODS_ID);
        otherGoods.setStoreId(OTHER_STORE_ID);

        when(goodsService.getById(GOODS_ID)).thenReturn(otherGoods);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            GoodsOperationDTO dto = new GoodsOperationDTO();
            ServiceException exception = assertThrows(ServiceException.class,
                    () -> controller.update(dto, GOODS_ID));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
            verify(goodsService, never()).editGoods(any(), any());
        }
    }

    @Test
    @DisplayName("商家修改本店商品时正常调用业务服务")
    void updateOwnStoreGoodsSuccess() {
        Goods ownGoods = new Goods();
        ownGoods.setId(GOODS_ID);
        ownGoods.setStoreId(CURRENT_STORE_ID);

        when(goodsService.getById(GOODS_ID)).thenReturn(ownGoods);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            GoodsOperationDTO dto = new GoodsOperationDTO();
            controller.update(dto, GOODS_ID);

            verify(goodsService).editGoods(eq(dto), eq(GOODS_ID));
        }
    }
}

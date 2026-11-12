package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import cn.lili.modules.distribution.mapper.DistributionInviteMapper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dto.DistributionInviteCardSetting;
import cn.lili.modules.system.entity.dto.DistributionPromotionPosterSetting;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 分销员推广海报与邀请卡支持类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class DistributionPromotionSupportTest {

    private DistributionPromotionPosterSettingSupport posterSettingSupport;
    private DistributionInviteCardSettingSupport inviteCardSettingSupport;
    private DistributionInviteMapper inviteMapper;
    private MemberService memberService;
    private DistributionPromotionSupport promotionSupport;

    @BeforeEach
    void setUp() {
        posterSettingSupport = mock(DistributionPromotionPosterSettingSupport.class);
        inviteCardSettingSupport = mock(DistributionInviteCardSettingSupport.class);
        inviteMapper = mock(DistributionInviteMapper.class);
        memberService = mock(MemberService.class);

        promotionSupport = new DistributionPromotionSupport(
                posterSettingSupport,
                inviteCardSettingSupport,
                inviteMapper,
                memberService
        );
    }

    @Test
    @DisplayName("测试获取推广海报信息")
    void testGetPromotionPoster() {
        Distribution distribution = new Distribution();
        distribution.setId("D100");
        distribution.setMemberId("M100");
        distribution.setMemberName("默认分销员");

        DistributionPromotionPosterSetting setting = new DistributionPromotionPosterSetting();
        setting.setMemberInfoVisible(true);
        setting.setBackgroundImage("https://example.com/bg.png");
        setting.setSlogan("邀请好友 共享好礼");
        setting.setTextColor("#FFFFFF");

        when(posterSettingSupport.readSetting()).thenReturn(setting);

        Member member = new Member();
        member.setId("M100");
        member.setNickName("昵称100");
        member.setFace("https://example.com/face.png");
        when(memberService.getById("M100")).thenReturn(member);

        DistributionPromotionPosterVO vo = promotionSupport.getPromotionPoster(distribution);

        assertNotNull(vo);
        assertEquals("D100", vo.getDistributionId());
        assertEquals("昵称100", vo.getMemberName());
        assertEquals("https://example.com/face.png", vo.getMemberAvatar());
        assertEquals("bind,D100", vo.getShareScene());
    }

    @Test
    @DisplayName("测试获取分销邀请卡信息")
    void testGetInviteCard() {
        Distribution distribution = new Distribution();
        distribution.setId("D200");
        distribution.setMemberId("M200");
        distribution.setInviteCode("INV200");
        distribution.setMemberName("默认分销员2");

        DistributionInviteCardSetting setting = new DistributionInviteCardSetting();
        setting.setMemberInfoVisible(true);
        setting.setBackgroundImage("https://example.com/card_bg.png");
        setting.setSlogan("加入我们");
        setting.setTextColor("#000000");

        when(inviteCardSettingSupport.readSetting()).thenReturn(setting);

        Member member = new Member();
        member.setId("M200");
        member.setNickName("小明");
        member.setFace("https://example.com/avatar.png");
        when(memberService.getById("M200")).thenReturn(member);

        DistributionInviteCardVO vo = promotionSupport.getInviteCard(distribution);

        assertNotNull(vo);
        assertEquals("D200", vo.getDistributionId());
        assertEquals("INV200", vo.getInviteCode());
        assertEquals("小明", vo.getMemberName());
        assertEquals("recruit_D200", vo.getShareScene());
    }

    @Test
    @DisplayName("测试分页查询受邀人")
    void testPageInvitees() {
        DistributionInviteeQueryDTO query = new DistributionInviteeQueryDTO();
        query.setPageNumber(1);
        query.setPageSize(10);

        when(inviteMapper.countInvitees(eq("D100"), nullable(Date.class), nullable(Date.class))).thenReturn(1L);
        when(inviteMapper.sumInviteReward(eq("D100"), nullable(Date.class), nullable(Date.class))).thenReturn(BigDecimal.valueOf(50.0));
        when(inviteMapper.listInvitees(eq("D100"), nullable(Date.class), nullable(Date.class), eq(0L), eq(10L)))
                .thenReturn(Collections.emptyList());

        DistributionInvitePageVO pageVO = promotionSupport.pageInvitees("D100", query);

        assertNotNull(pageVO);
        assertEquals(1L, pageVO.getTotal());
        assertEquals(BigDecimal.valueOf(50.0), pageVO.getTotalInviteReward());
    }
}

package cn.lili.security;

import cn.hutool.core.util.StrUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.SecretKeyUtil;
import cn.lili.common.utils.ResponseUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Chopper
 */
@Slf4j
public class StoreAuthenticationFilter extends BasicAuthenticationFilter {

    private final Cache cache;

    public StoreAuthenticationFilter(AuthenticationManager authenticationManager,
                                     Cache cache) {
        super(authenticationManager);
        this.cache = cache;
    }

    @SneakyThrows
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {

        String accessToken = request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
        if (StrUtil.isBlank(accessToken)) {
            chain.doFilter(request, response);
            return;
        }
        try {
            UsernamePasswordAuthenticationToken authentication = getAuthentication(accessToken, response);
            SecurityContextHolder.getContext().setAuthentication(authentication);
        } catch (Exception e) {
            log.error(e.getMessage());
        }

        chain.doFilter(request, response);
    }


    /**
     * 获取token信息
     *
     * @param jwt
     * @param response
     * @return
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String jwt, HttpServletResponse response) {

        try {
            Claims claims
                    = Jwts.parser()
                    .setSigningKey(SecretKeyUtil.generalKeyByDecoders())
                    .parseClaimsJws(jwt).getBody();
            //获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            AuthUser authUser = new Gson().fromJson(json, AuthUser.class);

            //校验redis中是否有权限
            if (cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.STORE) + jwt)) {
                //用户角色
                List<GrantedAuthority> auths = new ArrayList<>();
                auths.add(new SimpleGrantedAuthority("ROLE_" + authUser.getRole().name()));
                //构造返回信息
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(authUser.getUsername(), null, auths);
                authentication.setDetails(authUser);
                return authentication;
            }
            ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
            return null;
        } catch (ExpiredJwtException e) {
            log.debug("user analysis exception:", e);
        } catch (Exception e) {
            log.error("user analysis exception:", e);
        }
        return null;
    }
}

